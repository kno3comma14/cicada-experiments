(ns cicada-experiments.core-test
  (:require [clojure.test :refer :all]
            [cicada-experiments.core :as core]))

(deftest sc-reduce32-test
  (testing "sc_reduce32 functionality is working fine."
    (let [first-hex-input "f9a0e73d3cd533368f75ff63cbd97b2100beffbc339cdfa5c203c1a022d9cf11"
          second-hex-input "fcc659eca955591729400f38c6917e8ae0af1405b2a03b5a7074a74b0946a8d5"
          first-expected-result "0ccdf1e0217221deb8d807c1ecdf9c0c00beffbc339cdfa5c203c1a022d9cf01"
          second-expected-result "f303de33534d6a9e46497cf177e12b7bdfaf1405b2a03b5a7074a74b0946a805"
          first-actual-result (core/sc-reduce32 first-hex-input)
          second-actual-result (core/sc-reduce32 second-hex-input)]
      (is (and (= first-expected-result first-actual-result)
               (= second-expected-result second-actual-result))))))
