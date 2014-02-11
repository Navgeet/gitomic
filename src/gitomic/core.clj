(ns gitomic.core
  "Top level interface for gitomic repos."
  (:require [datomic.api :as d :refer [db q]]
            [gitomic.schema :refer [schema]]
            [gitomic.util :as util]))

(def mem-uri "datomic:mem://gitomic")
(def db-conn (atom nil))

(defn ensure-schema [conn]
  @(d/transact conn schema))

(defn ensure-db [db-uri]
  (let [db-uri (or db-uri mem-uri)
        newdb? (d/create-database db-uri)
        conn (d/connect db-uri)]
    (when newdb? (ensure-schema conn))
    (swap! db-conn (constantly conn))
    conn))

;; Public Api
(defmulti obj-tx-data
  "Returns transaction data for a git obj (commit, tree, blob, tag)."
  :type)

(defmethod obj-tx-data :blob [{:keys [sha content]} db]
  (let [sha-id ((util/index->id-fn db :git/sha) sha)]
    (util/tx-data-for-map sha-id {:git/sha sha
                                  :git/type :blob
                                  :blob/content content} db)))

(defmethod obj-tx-data :tree [{:keys [sha nodes]} db]
  (let [sha->id (util/index->id-fn db :git/sha)
        filename->id (util/index->id-fn db :file/name)
        sha-id (sha->id sha)
        node-data (fn [[type sha filename]]
                    (let [sha-id (sha->id sha)
                          filenameid (filename->id filename)
                          nodeid (or (and (not (util/tempid? sha-id))
                                          (not (util/tempid? filenameid))
                                          (ffirst (q '[:find ?e :in $ ?filename ?id
                                                       :where
                                                       [?e :node/filename ?filename]
                                                       [?e :node/object ?id]]
                                                     db filenameid sha-id)))
                                     (d/tempid :db.part/user))]
                      [nodeid
                       (cond-> []
                               (util/tempid? filenameid) (conj [:db/add filenameid :file/name filename])
                               (util/tempid? sha-id) (conj {:db/id sha-id
                                                            :git/type (keyword type)
                                                            :git/sha sha})
                               (util/tempid? nodeid) (conj {:db/id nodeid
                                                            :node/object sha-id
                                                            :node/filename filenameid}))]))
        [nodes nodes-tx-data] (reduce (fn [[nodes nodes-tx-data] [node node-tx-data]]
                                        [(conj nodes node) (into nodes-tx-data node-tx-data)])
                                      [[] []]
                                      (map node-data nodes))]
    (into nodes-tx-data
          (util/tx-data-for-map sha-id {:git/sha sha
                                        :git/type :tree
                                        :tree/nodes nodes} db))))

(defmethod obj-tx-data :commit [{:keys [sha parents message authored-at
                                        committed-at tree author committer]} db]
  (let [sha->id (util/index->id-fn db :git/sha)
        treeid (sha->id tree)
        author->id (fn [name email]
                     (or (ffirst (q '[:find ?e :in $ ?name ?email
                                      :where
                                      [?e :author/name ?name]
                                      [?e :author/email-address ?email]]
                                    db name email))
                         (d/tempid :db.part/user)))
        authorid (author->id (:name author) (:email-address author))
        committerid (author->id (:name committer) (:email-address committer))
        parent-data (fn [parent-sha]
                      (let [parentid (sha->id parent-sha)]
                        [parentid (util/tx-data-for-map parentid {:git/sha parent-sha
                                                                  :git/type :commit} db)]))
        [parents parents-tx-data] (reduce (fn [[parents parents-tx-data] [parent parent-tx-data]]
                                        [(conj parents parent) (into parents-tx-data parent-tx-data)])
                                      [[] []]
                                      (map parent-data parents))]
    (into (cond-> parents-tx-data
                  (util/tempid? treeid) (conj {:db/id treeid
                                               :git/sha tree
                                               :git/type :tree})
                  (util/tempid? authorid) (conj {:db/id authorid
                                                 :author/name (:name author)
                                                 :author/email-address (:email-address author)})
                  (util/tempid? committerid) (conj {:db/id committerid
                                                    :author/name (:name committer)
                                                    :author/email-address (:email-address committer)}))
          (util/tx-data-for-map (sha->id sha) {:git/type :commit
                                               :git/sha sha
                                               :commit/message message
                                               :commit/authoredAt authored-at
                                               :commit/committedAt committed-at
                                               :commit/author authorid
                                               :commit/committer committerid
                                               :commit/parents parents
                                               :commit/tree treeid} db))))

(defn transact-obj
  "Transacts a git obj to a datomic db."
  [obj conn]
  (let [conn (or conn @db-conn)]
    (d/transact conn (obj-tx-data obj (db conn)))))
