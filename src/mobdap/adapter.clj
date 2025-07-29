(ns mobdap.adapter
  (:require
   [cheshire.core :as json]
   [clojure.string :as string]
   [taoensso.timbre :as log])
  (:import
   [java.io
    BufferedReader
    BufferedWriter
    Reader
    Writer]))

(defn- parse-content-length! [^Reader reader]
  (let [length (loop [line (.readLine reader)]
                 (if line
                   (let [values (re-find #"content-length\s*:\s*([0-9]+)" (string/trim (string/lower-case line)))
                         length (second values)]
                     (if length
                       length
                       (recur (.readLine reader))))
                   (recur (.readLine reader))))
        _ (.readLine reader)] ; skip empty line
    (when length
      (Integer/parseInt length))))

(defn read-message! [adapter]
  (let [reader (:reader adapter)
        length (parse-content-length! reader)]
    (when length
      (let [buffer (char-array length)]
        (.read reader buffer 0 length)
        (let [message (json/parse-string (String/new buffer) keyword)]
          (log/info "Received Message:" message)
          message)))))

(defn send-message! [adapter message]
  (let [writer (:writer adapter)
        json-str (json/generate-string message)
        length   (count json-str)]
    (log/info "Send Message:" message)
    (doto writer
      (.write (str "Content-Length: " length "\r\n\r\n" json-str))
      (.flush))))

(defn create-adapter [^Reader in ^Writer out]
  (let [reader (BufferedReader/new in)
        writer (BufferedWriter/new out)]
    {:reader reader :writer writer}))

(defn create-stdio-adapter []
  (create-adapter *in* *out*))

