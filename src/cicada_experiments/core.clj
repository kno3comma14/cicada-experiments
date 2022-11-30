(ns cicada-experiments.core
  (:require [clojure.math.numeric-tower :as math]
            [bites.core :as bc]
            [buddy.core.codecs :as codecs])
  (:import org.bouncycastle.jcajce.provider.digest.SHA3$DigestSHA3))

(def t (atom 0))
(def Q (atom []))
(def x (atom 0))

(def q (biginteger (- (math/expt 2 255) 19)))

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

(defn get-bit
  [data position]
  (let [byte-position (quot position 8)
        bit-position (mod position 8)
        byte-value (nth data byte-position)
        pivote (- 8 (+ bit-position 1))]
    (and (bit-shift-right byte-value pivote) 0x0001)))

(defn decode-byte-array [input]
  (let [b 256]
    (biginteger (reduce +
                        (for [i (range 0 b)]
                          (* (math/expt 2 i) (get-bit input i)))))))

(defn exp-mod
  [b e m]
  (if (= e 0)
    1
    (do
      (reset! t (mod (math/expt (exp-mod b (quot e 2) m) 2) m))
      (when (not= (and e 1) 0)
        (reset! t (mod (* @t b) m)))
      @t)))

(defn inv
  [x]
  (exp-mod x (- q 2) q))

(def d (* -121665 (inv 121666)))
(def I (exp-mod 2 (quot (- q 1) 4) q))

(defn x-recovery
  [y]
  (let [xx (* (- (math/expt y 2) 1) (inv (+ (* d y y) 1)))]
    (reset! x (exp-mod xx (quot (+ q 3) 8) q))
    (when (not= (mod (- (math/expt @x 2) xx) q) 0)
      (reset! x (mod (* @x I) q)))
    (when (not= (mod @x 2) 0)
      (reset! x (- q @x)))
    @x))

(def By (biginteger (* 4 (inv 5))))
(def Bx (biginteger (x-recovery By)))
(def B [(biginteger (mod Bx q)) (biginteger (mod By q))])

(defn edwards
  [P Q]
  (let [x1 (nth P 0)
        y1 (nth P 1)
        x2 (nth Q 0)
        y2 (nth Q 1)
        x3 (* (+ (* x1 y2) (* x2 y1)) (inv (+ 1 (* d x1 x2 y1 y2))))
        y3 (* (+ (* y1 y2) (* x1 x2)) (inv (- 1 (* d x1 x2 y1 y2))))]
    [(mod x3 q) (mod y3 q)]))

(defn scalar-multiplication
  [P e]
  (if (= e 0)
    [0 1]
    (do
      (reset! Q (scalar-multiplication P (quot e 2)))
      (reset! Q (edwards @Q @Q))
      (when (not= (and e 1) 0)
        (reset! Q (edwards @Q P)))
      @Q)))

(defn encode-point
  [P]
  (let [x (nth P 0)
        y (biginteger (nth P 1))
        semi-bits (for [i (range 0 255)]
                    (.shiftRight y i))
        bits (apply list (concat semi-bits (list (and x 1))))
        placed-bytes (for [i (range 0 32)]
                       (biginteger (reduce + (for [j (range 0 8)]
                                               (.shiftLeft (biginteger (nth bits (+ (* i 8) j))) j)))))
        pre-result (byte-array placed-bytes)]
    (codecs/bytes->hex pre-result)))

(defn ->public-key [private-key]
  (let [private-key-byte-array (codecs/hex->bytes private-key)
        a (decode-byte-array private-key-byte-array)
        A (scalar-multiplication B a)]
    (encode-point A)))






