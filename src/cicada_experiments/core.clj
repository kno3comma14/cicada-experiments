(ns cicada-experiments.core
  (:require [clojure.math.numeric-tower :as math]
            [bites.core :as bc]
            [buddy.core.codecs :as codecs])
  (:import org.bouncycastle.jcajce.provider.digest.SHA3$DigestSHA3))

(defn bytes->int
  [^bytes bytes & {:keys [little-endian]
                   :or {little-endian true}}]
  (let [b (if little-endian (reverse bytes) bytes)]
    (->> b
         (cons (byte 0))
         (byte-array)
         (biginteger))))

(defn- complement-solution [k]
  (str (apply str (take (- 64 (count k)) (repeat "0"))) k))

(defn to-byte-array [hex-input-string]
  (byte-array (map #(read-string (str "0x" (first %) (second %))) (apply list (partition 2 hex-input-string)))))

(defn- generate-random-hex-term []
  (let [hex-characters "0123456789abcdef"
        index (rand-int 16)]
    (get hex-characters index)))

(defn create-hex-private-spend-key []
  (loop [i 0
         result ""]
    (if (>= i 64)
      result
      (recur (inc i)
             (str result (generate-random-hex-term))))))

(defn execute-keccak256 [hex-input]
  (let [sha3-digest (SHA3$DigestSHA3. 256)
        _ (.update sha3-digest (codecs/hex->bytes hex-input))
        pre-result (.digest sha3-digest)]
    (codecs/bytes->hex pre-result)))

(defn sc-reduce32 [s]
  (let [n (bytes->int (to-byte-array s))
        l (biginteger (+ (math/expt 2 252) 27742317777372353535851937790883648493))
        reduced-input (biginteger (.mod n l))
        pre-result (bc/to-bytes reduced-input)
        result (.toString (bytes->int pre-result) 16)]
    (complement-solution result)))

(defn generate-private-spend-key [hex-private-spend-key]
    (sc-reduce32 hex-private-spend-key))

(defn generate-private-view-key [private-spend-key]
  (let [prepared-spend-key (execute-keccak256 private-spend-key)
        private-view-key (sc-reduce32 prepared-spend-key)]
    private-view-key))
