(ns gitomic.main
  (:require [aleph.http :refer [start-http-server
                                wrap-ring-handler]]
            [gitomic.http :as http]))

(defn -main [& port]
  (println "starting gitomic")
  (start-http-server (wrap-ring-handler http/gitomic-http-handler) {:port (or port 3000)}))
