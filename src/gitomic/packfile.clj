(ns gitomic.packfile
  (:import [java.util.zip Inflater]))

(def obj-types {0 :invalid
                1 :commit
                2 :tree
                3 :blob
                4 :tag
                5 :invalid
                6 :delta-with-offset
                7 :delta-with-ref})

(defn bit-seq [byte]
  (vec (map #(bit-test byte %) [7 6 5 4 3 2 1 0])))

(defn read-obj
  "Reads the next object from the inputStream. Needs length after decompression."
  [ins len]
  (let [inflater (Inflater.)
        out (byte-array len)
        byte (byte-array 1)]
    ;; git stores the length after inflation
    ;; so we read bytes one by one from the inputStream
    ;; till we have have read len no. of bytes
    (loop [bytes-out 0]
      (let [_ (.read ins byte 0 1)
            _ (.setInput inflater byte)
            bytes-out (+ (.inflate inflater out bytes-out (- len bytes-out))
                         bytes-out)]
        (if (and (= bytes-out len) (.finished inflater))
          out
          (recur bytes-out))))))

(defn add-bit-to-sum [sum bits index]
  (if (bits (- (count bits) index 1))
    (+ sum (int (java.lang.Math/pow 2 index)))
    sum))

(defn bits->int
  "Convert a bit sequence to an int."
  [bits]
  (loop [index (- (count bits) 1)
         sum 0]
    (if (= index 0)
      (add-bit-to-sum sum bits 0)
      (recur (- index 1) (add-bit-to-sum sum bits index)))))

(defn decode-obj
  "Decodes the next object from the packfile inputstream."
  ([ins] (decode-obj ins [] nil))
  ([ins obj-len-bits obj-type]
     (let [bits (bit-seq (.readByte ins))
           obj-type (or obj-type (obj-types (bits->int (subvec bits 1 4))))
           obj-len-bits (if (empty? obj-len-bits)
                          (into (subvec bits 4) obj-len-bits)
                          (into (subvec bits 1) obj-len-bits))]
       (if (bits 0)
         (decode-obj ins obj-len-bits obj-type)
         {:type obj-type :length (bits->int obj-len-bits) :data (read-obj ins
                                                                          (bits->int obj-len-bits))}))))

(defn decode-pack
  "Decodes a sequence of objects from the packfile."
  [ins]
  (.skipBytes ins 8)
  (let [num (.readInt ins)]
    (vec (repeatedly num #(decode-obj ins)))))
