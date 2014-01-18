(ns gitomic.http
  (:require [aleph.http :as http]
            [lamina.core :as lamina]
            [compojure.core :refer [defroutes GET POST]]
            [compojure.route :as route]
            [ring.middleware.params :refer [wrap-params]]))

(defroutes gitomic-http-routes
  "Ring handler to serve git repos using git smart HTTP protocol."
  (GET ["/:repo/info/refs"] {{service "service"} :query-params {repo :repo} :params}
       {:status 200
        :headers {"Content-Type" "application/x-git-upload-pack-advertisement"}
        :body (slurp "refs")})
  (GET ["/"] {} "Hello world!")
  (route/not-found "Page not found"))

(def gitomic-http-handler (wrap-params gitomic-http-routes))
