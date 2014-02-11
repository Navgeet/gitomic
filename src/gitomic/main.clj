(ns gitomic.main
  (:require [gitomic.core :refer [ensure-db]]
            [gitomic.http :refer [server-start]]))

(defn -main [& [port db-uri]]
  (let [port (or port 3000)
        conn (ensure-db db-uri)]
    (println (str "Starting Gitomic smart-http server on localhost:" port))
    (println (str "git remote add gitomic http://loclhost:" port))
    (println "git push gitomic master")
    (server-start port)))
