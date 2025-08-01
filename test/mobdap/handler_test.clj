(ns mobdap.handler-test
  (:require
   [clojure.test :refer [deftest is]]
   [matcher-combinators.test :refer [match?]]
   [mobdap.handler :as handler]))

(defn- make-handler [m]
  (let [create-handler #'mobdap.handler/create-handler
        h              (create-handler nil)]
    (into {} (map (fn [[k v]] (assoc-in h (if (seq? k) k [k]) v)) m))))

(deftest test-find-breakpoint-id
  (let [find-breakpoint-id #'mobdap.handler/find-breakpoint-id
        handler (make-handler {:breakpoints {"main/main.lua" [{:id 1337 :line 12} {:id 42 :line 24}]}})]
    (is (= (find-breakpoint-id handler "main/main.lua" 12) 1337))
    (is (= (find-breakpoint-id handler "main/main.lua" 24) 42))
    (is (= (find-breakpoint-id handler "main/main.lua" 1) nil))))

(deftest test-parse-heap-value
  (let [parse-heap-value #'mobdap.handler/parse-heap-value]
    (is (match? (parse-heap-value "table: 0x12345") {:type :table :addr "0x12345"}))
    (is (match? (parse-heap-value "Script: 0xFFFFF") {:type :Script :addr "0xFFFFF"}))
    (is (= (parse-heap-value "0xFFFFF") nil))))

(deftest test-parse-inner-value
  (let [parse-inner-value #'mobdap.handler/parse-inner-value
        handler (make-handler {[:counter :vars] (atom 0)})]
    (is (match? (parse-inner-value handler nil 5)
                {:name nil
                 :type :constant
                 :value 5}))
    (is (match? (parse-inner-value handler nil nil)
                {:name nil
                 :type :constant
                 :value nil}))
    (is (match? (parse-inner-value handler nil [1 2 3])
                {:id 1
                 :name nil
                 :type :table
                 :addr nil
                 :value [{:name nil
                          :type :constant
                          :value 1}
                         {:name nil
                          :type :constant
                          :value 2}
                         {:name nil
                          :type :constant
                          :value 3}]}))
    (is (match? (parse-inner-value handler nil {:a 1 :b 2 :c [1 2]})
                {:id 2
                 :name nil
                 :type :table
                 :addr nil
                 :value {:a {:name :a
                             :type :constant
                             :value 1}
                         :b {:name :b
                             :type :constant
                             :value 2}
                         :c {:id 3
                             :name :c
                             :type :table
                             :addr nil
                             :value [{:name nil
                                      :type :constant
                                      :value 1}
                                     {:name nil
                                      :type :constant
                                      :value 2}]}}}))))

(deftest test-parse-value
  (let [parse-value #'mobdap.handler/parse-value
        handler (make-handler {[:counter :vars] (atom 0)})]
    (is (match? (parse-value handler :a [nil])
                {:name :a
                 :type :constant
                 :value nil}))
    (is (match? (parse-value handler :a [3 "3"])
                {:name :a
                 :type :constant
                 :value 3}))
    (is (match? (parse-value handler :a [{:b 1} "table: 0xabcdef"])
                {:id 1
                 :name :a
                 :type :table
                 :addr "0xabcdef"
                 :value {:b {:name :b
                             :type :constant
                             :value 1}}}))
    (is (match? (parse-value handler :a [["5"] "table: 0xabcdef"])
                {:id 2
                 :name :a
                 :type :table
                 :addr "0xabcdef"
                 :value [{:name 0
                          :type :constant
                          :value "5"}]}))))

(deftest test-register-vars
  (let [register-vars #'mobdap.handler/register-vars
        handler (make-handler {[:counter :vars] (atom 0)})]
    (register-vars handler {:a {:name :a
                                :type :constant
                                :value 5}
                            :b {:id 1
                                :name :b
                                :type :table
                                :addr "0xabcdef"
                                :value {:a {:id 2
                                            :type :table
                                            :name :a
                                            :addr nil
                                            :value [{:name 0
                                                     :type :constant
                                                     :value 1337}]}}}})
    (is (match? (@(:var-index handler) 2)
                {:id 2
                 :type :table
                 :name :a
                 :addr nil
                 :value [{:name 0
                          :type :constant
                          :value 1337}]}))))

