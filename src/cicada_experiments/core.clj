(ns cicada-experiments.core
  (:require [clojure.math.numeric-tower :as math]
            [bites.core :as bc]))

(defn- bytes->int
  [^bytes bytes & {:keys [little-endian]
                   :or {little-endian true}}]
  (let [b (if little-endian (reverse bytes) bytes)]
    (->> b
         (cons (byte 0))
         (byte-array)
         (biginteger))))

(defn- complement-solution [k]
  (str (apply str (take (- 64 (count k)) (repeat "0"))) k))

(defn- to-byte-array [hex-input-string]
  (byte-array (map #(read-string (str "0x" (first %) (second %))) (apply list (partition 2 hex-input-string)))))

(defn sc-reduce32 [s]
  (let [n (bytes->int (to-byte-array s))
        l (biginteger (+ (math/expt 2 252) 27742317777372353535851937790883648493))
        reduced-input (biginteger (.mod n l))
        pre-result (bc/to-bytes reduced-input)
        result (.toString (bytes->int pre-result) 16)]
    (complement-solution result)))
