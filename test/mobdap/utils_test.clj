(ns mobdap.utils-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [mobdap.utils :refer [float-to-string map-over-map to-int]]))

(deftest test-to-int
  (testing "int just returns as is"
    (is (= 1337 (to-int 1337))))
  (testing "strings convert properly"
    (is (= 42 (to-int "42")))))

(deftest test-map-over-map
  (testing "square every item"
    (is (match? {:a 1 :b 4 :c 9 :d 16} (map-over-map #(* %2 %2) {:a 1 :b 2 :c 3 :d 4}))))
  (testing "turn value to key"
    (is (match? {:a :a :b :b :c :c} (map-over-map (fn [k _] k) {:a 1 :b 2 :c 3})))))

(deftest test-float-to-string
  (testing "make nice looking float strings"
    (is "1.337" (float-to-string 1.337))))

