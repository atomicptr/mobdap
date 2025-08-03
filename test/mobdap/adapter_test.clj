(ns mobdap.adapter-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [matcher-combinators.test :refer [match?]]
   [mobdap.adapter :as adapter])
  (:import
   [java.io BufferedReader StringReader StringWriter]))

(deftest test-parse-content-length!
  (let [parse-content-length! #'adapter/parse-content-length!]
    (testing "parses content length from valid input"
      (let [input "Content-Length: 17\r\n\r\n{\"hello\":\"World\"}"
            reader (BufferedReader/new (StringReader/new input))]
        (is (= 17 (parse-content-length! reader)))))
    (testing "parses content length case insensitively"
      (let [input "content-length: 1337\r\n\r\n"
            reader (BufferedReader/new (StringReader/new input))]
        (is (= 1337 (parse-content-length! reader)))))
    (testing "handles multiple lines before content length"
      (let [input "some-key: some-value\r\nsome-other-key: some-other-value\r\ncontent-length: 1234\r\n\r\n"
            reader (BufferedReader/new (StringReader/new input))]
        (is (= 1234 (parse-content-length! reader)))))
    (testing "skips empty line"
      (let [input "Content-Length: 21\r\n\r\n{\"hello\":\"World\"}"
            reader (BufferedReader/new (StringReader/new input))]
        (parse-content-length! reader)
        (is (= (.readLine reader) "{\"hello\":\"World\"}"))))))

(deftest test-read-message!
  (testing "reads valid json data"
    (let [input "Content-Length: 17\r\n\r\n{\"hello\":\"World\"}"
          r (StringReader/new input)
          adapter (adapter/create-adapter r (StringWriter/new))]
      (is (match? {:hello "World"} (adapter/read-message! adapter))))))

(deftest test-send-message!
  (testing "sends valid json"
    (let [message {:hello "World"}
          w (StringWriter/new)
          adapter (adapter/create-adapter (StringReader/new "") w)]
      (adapter/send-message! adapter message)
      (let [output (str w)]
        (is (= "Content-Length: 17\r\n\r\n{\"hello\":\"World\"}" output))))))

