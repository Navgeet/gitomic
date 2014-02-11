(ns gitomic.util
  (:require [datomic.api :as d])
  (:import [java.security MessageDigest]
           [java.util Date]))

;;; Parsing git object notation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; example commit - git cat-file -p
;;tree d81cd432f2050c84a3d742caa35ccb8298d51e9d
;;author Rich Hickey <richhickey@gmail.com> 1348842448 -0400
;;committer Rich Hickey <richhickey@gmail.com> 1348842448 -0400

;;; or

;;tree ba63180c1d120b469b275aef5da479ab6c3e2afd
;;parent c3bd979cfe65da35253b25cb62aad4271430405c
;;;maybe more parents
;;author Rich Hickey <richhickey@gmail.com> 1348869325 -0400
;;committer Rich Hickey <richhickey@gmail.com> 1348869325 -0400
;;then blank line
;;then commit message

;;example tree
;;100644 blob ee508f768d92ba23e66c4badedd46aa216963ee1	.gitignore
;;100644 blob b60ea231eb47eb98395237df17550dee9b38fb72	README.md
;;040000 tree bcfca612efa4ff65b3eb07f6889ebf73afb0e288	doc
;;100644 blob 813c07d8cd27226ddd146ddd1d27fdbde10071eb	epl-v10.html
;;100644 blob f8b5a769bcc74ee35b9a8becbbe49d4904ab8abe	project.clj
;;040000 tree 6b880666740300ac57361d5aee1a90488ba1305c	src
;;040000 tree 407924e4812c72c880b011b5a1e0b9cb4eb68cfa	test

(defn- hexdigest [binary-id]
  (let [sig (.toString (BigInteger. 1 binary-id) 16)
        padding (apply str (repeat (- 40 (count sig)) "0"))]
    (str padding sig)))

(defn obj-sha1
  "Calculates the SHA-1 hexdigest by prepending object type,
object length and a null byte to the objects's contents."
  [type bytes length]
  (let [md (MessageDigest/getInstance "SHA-1")]
    (.reset md)
    (.update md (.getBytes (str (name type) " " length)))
    (.update md (byte 0))
    (.update md bytes)
    (hexdigest (.digest md))))

(defn str->time [s]
  (let [[_ seconds op hrs mins] (re-matches #"(\d+) ([-+])(\d\d)(\d\d)" s)
        op (if (= op "+") + -)]
    (Date. (* 1000 (op (Long/parseLong seconds) (+ (* 3600 (Integer/parseInt hrs))
                                                     (* 60 (Integer/parseInt mins))))))))


(def commit-regexp #"tree (\S+)((\nparent \S+)*)
author (.+) <(\S+)> ([-+0-9 ]+)
committer (.+) <(\S+)> ([-+0-9 ]+)

(.*)
")

(defn decode-tree [tree sha]
  (with-open [ins (clojure.java.io/input-stream tree)]
    (let [read-node (fn [ins]
                      (let [mode (byte-array 6)
                            _ (.read ins mode 0 6)
                            mode (String. mode)
                            _ (.skip ins 1)
                            name (reduce #(str %1 (char %2)) ;; read filename till null byte
                                         ""
                                         (take-while #(> % 0) (repeatedly #(.read ins))))
                            obj-id-binary (byte-array 20)
                            _ (.read ins obj-id-binary 0 20)]
                        [(condp = mode
                           "040000" :tree
                           "160000" :commit
                           :blob)
                         (hexdigest obj-id-binary)
                         name]))]
      {:type :tree
       :nodes (loop [nodes []]
                (if (= (.available ins) 0)
                  nodes
                  (recur (conj nodes (read-node ins)))))
       :sha sha})))

(defn decode-commit [commit sha]
  (let [match (re-matches commit-regexp commit)
        tree (second match)
        [author-name author-email authored-at-str
         committer-name committer-email committed-at-str
         message] (subvec match (- (count match) 7))
        parents (seq (subvec (clojure.string/split (match 2) #"\nparent ") 1))]
    {:type :commit
     :sha sha
     :parents parents
     :message message
     :author {:name author-name
              :email-address author-email}
     :authored-at (str->time authored-at-str)
     :committer {:name committer-name
                 :email-address committer-email}
     :committed-at (str->time committed-at-str)
     :tree tree}))

(defn decode-git-obj
  "Decodes objects from git's object format to maps."
  [type obj length]
  (let [sha (obj-sha1 type obj length)]
   (condp = type
     :blob {:type :blob
            :sha sha
            :content obj}
     :tree (decode-tree obj sha)
     :commit (decode-commit (String. obj) sha))))

;;; Functions for dealing with datomic entities
(defn index-get-id
  [db attr v]
  (let [d (first (d/index-range db attr v nil))]
    (when (and d (= (:v d) v))
      (:e d))))

(defn index->id-fn
  [db attr]
  (memoize
   (fn [x]
     (or (index-get-id db attr x)
         (d/tempid :db.part/user)))))

(def tempid? map?)

(defn missing-keys
  "Returns the set of attributes missing from the given entity's attributes."
  [eid attr-keys db]
  (clojure.set/difference (set attr-keys) (set (keys (d/entity db eid)))))

(defn tx-data-for-map
  "Returns transaction data for an entity and a map of attributes and values.
  Checks if the entity exists. If yes, then add only keys which aren't present."
  [eid attr-map db]
  (if (tempid? eid)
    [(merge attr-map {:db/id eid})]
    (reduce #(conj %1 [:db/add eid %2 (attr-map %2)]) [] (missing-keys eid (keys attr-map) db))))
