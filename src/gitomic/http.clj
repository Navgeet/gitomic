(ns gitomic.http
  (:require [gitomic.core :as core]
            [clojure.string :as string]
            [compojure.core :refer [defroutes GET POST]]
            [compojure.route :as route]
            [ring.middleware.params :refer [wrap-params]]
            [aleph.http :refer [start-http-server
                                wrap-ring-handler]]
            [gitomic.packfile :as pack]))

(alter-var-root #'clout.core/re-groups*
                (constantly (fn [matcher]
                              (remove nil? (for [i (range (.groupCount matcher))]
                                             (.group matcher (int (inc i))))))))

(def res (atom "foo"))

(defn discover-refs [service]
  (slurp (str "emptyrefs-" service)))

(defn read-pkt-len
  "Reads four bytes from the given inputstream as a hex encoded length."
  [ins]
  (let [header-bytes (byte-array 4)]
    (.read ins header-bytes 0 4)
    (Integer/parseInt (String. header-bytes) 16)))

(defn parse-pkt-line
  "Parses a packet line format response.
  A packet line response is of the form -
  [4 byte hex encoded length][line][optional linefeed]
  Returns a vector of vectors, where each line is split by space,
  and the trailing linefeed stripped if present."
  [ins]
  (loop [lines []
         pkt-len (read-pkt-len ins)]
    (if (= pkt-len 0)
      lines
      (let [line-len (- pkt-len 4)
            line-bytes (byte-array line-len)
            _ (.read ins line-bytes 0 line-len)
            line-vec (-> line-bytes
                         ;; strip linefeed
                         String. string/trim-newline
                         ;; separate caps list after null byte
                         (string/split (re-pattern (str (char 0)))))
            line-vec (map #(string/split (string/triml %) #" ") line-vec)] ;; split each line by space
        (recur (into lines line-vec) (read-pkt-len ins))))))

(defn git-receive-pack-parse
  "Parses the data sent to git-receive-pack into a map of commands,
  capabilities requested by the client and objects sent in the packfile."
  [ins]
  (let [lines (parse-pkt-line ins)
        capabilities (lines 1)
        lines (into [(first lines)] (subvec lines 2))]
    {:commands lines :capabilities capabilities :objects (pack/decode-pack ins)}))

(defn git-receive-pack [ins]
  (let [{:keys [commands capabilities objects]} (git-receive-pack-parse ins)]
    (swap! res (constantly objects))))

(defroutes gitomic-http-routes
  "Ring handler to serve git repos using git smart HTTP protocol."
  (GET ["/:repo/info/refs", :repo #"([a-zA-Z0-9._]+/)*[a-zA-Z0-9._]+"]
       ;;TODO: improve this regex
       {{service "service"} :query-params {repo :repo} :params}
       (if (#{"git-receive-pack" "git-upload-pack"} service)
         {:status 200
          :headers {"Content-Type" (format "application/x-%s-advertisement" service)
                    "Cache-Control" "no-cache"}
          :body (discover-refs service)}
         {:status 403
          :headers {"Content-Type" "text/plain"
                    "Cache-Control" "no-cache"}
          :body "Service not available"}))
  (POST ["/:repo/git-receive-pack", :repo #"([a-zA-Z0-9._]+/)*[a-zA-Z0-9._]+"]
        {{:keys [repo]} :params body :body}
        (git-receive-pack body)
        {:status 200
         :headers {"Content-Type" "application/x-git-receive-pack-result"
                   "Cache-Control" "no-cache"}})
  (POST ["/:repo/git-upload-pack", :repo #"([a-zA-Z0-9._]+/)*[a-zA-Z0-9._]+"]
        {{:keys [repo]} :params body :body}
        (println (slurp body)))
  (route/not-found "Page not found"))

(defn server-start
  "Starts Gitomic smart-http server."
  [port]
  (start-http-server (-> gitomic-http-routes
                         wrap-params
                         wrap-ring-handler)
                     {:port port}))
