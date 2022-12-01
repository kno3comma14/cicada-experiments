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

(deftest execute-keccak256-test
  (testing "execute-keccak256 functionality working fine"
    (let [input "0ccdf1e0217221deb8d807c1ecdf9c0c00beffbc339cdfa5c203c1a022d9cf01"
          expected-result "6454a14f55aeca8ade0b5a3eeb240b149f6a572b397c8bec4c2cd6d58098156c"
          actual-result (core/execute-keccak256 input)]
      (is (= expected-result actual-result)))))

(deftest generate-private-view-key-test
  (testing "generate-private-view-key functionality working fine"
    (let [input "0ccdf1e0217221deb8d807c1ecdf9c0c00beffbc339cdfa5c203c1a022d9cf01"
          expected-result "d65cde21b75b5c7ad85e8c6cb349d1969e6a572b397c8bec4c2cd6d58098150c"
          actual-result (core/generate-private-view-key input)]
      (is (= expected-result actual-result)))))

(deftest ->public-key-test
  (testing "->public-key function working as expected"
    (let [input-private-key "f303de33534d6a9e46497cf177e12b7bdfaf1405b2a03b5a7074a74b0946a805"
          expected-result "d76344d2c5467758f0bcbf03925bc8bf4b659e163ec68c342c7ba94b9679a125"
          actual-result (core/->public-key input-private-key)]
      (is (= expected-result actual-result)))))

(deftest encode-point-test
  (testing "encode-point function working as expected"
    (let [input-x (biginteger 57558631862586115701998314212592984452098739536441416854074616191591557781808)
          input-y (biginteger 6007976670667633535842652653651559405949256554456092264774644708082919226127)
          input-point [input-x input-y]
          expected-result "0f3b913371411b27e646b537e888f685bf929ea7aab93c950ed84433f064480d"
          actual-result (core/encode-point input-point)]
      (is (= expected-result actual-result)))))
; 
(deftest scalar-multiplication-test
  (testing "scalar-multiplication function working as expected"
    (let [coord-x (biginteger 15112221349535400772501151409588531511454012693041857206046113283949847762202)
          coord-y (biginteger 46316835694926478169428394003475163141307993866256225615783033603165251855960)
          input-point [coord-x coord-y]
          input-scalar (biginteger 4090177687267471719164802180371655790974410769055056530206928641275872410231)
          expected-result [(biginteger 57558631862586115701998314212592984452098739536441416854074616191591557781808)
                           (biginteger 6007976670667633535842652653651559405949256554456092264774644708082919226127)]
          actual-result (core/scalar-multiplication input-point input-scalar)]
      (is (= expected-result actual-result)))))
