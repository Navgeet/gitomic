(defproject gitomic "0.1.0-SNAPSHOT"
  :description "git on Datomic"
  :url "https://github.com/Navgeet/gitomic"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"] ;; Thank you Rich Hickey!
                 [aleph "0.3.1"]
                 [compojure "1.1.6"]
                 [com.datomic/datomic-free "0.9.4497"]]
  :main gitomic.main)
