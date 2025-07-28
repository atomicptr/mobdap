(ns mobdap.dap.server
  (:require [cheshire.core :as json]
            [clojure.core.async :refer [chan go >!]]
            [clojure.string :as string]
            [taoensso.timbre :as log])
  (:import [java.io BufferedWriter BufferedReader]))

(defn- parse-content-length! [in]
  (let [length (loop [line (.readLine in)]
                 (if line
                   (let [values (re-find #"content-length\s*:\s*([0-9]+)" (string/trim (string/lower-case line)))
                         length (second values)]
                     (if length
                       length
                       (recur (.readLine in))))
                   (recur (.readLine in))))

        _ (.readLine in)] ; skip empty line
    (when length
      (Integer/parseInt length))))

(defn read-message! [in]
  (let [length (parse-content-length! in)]
    (when length
      (let [buffer (char-array length)]
        (.read in buffer 0 length)
        (json/parse-string (String/new buffer) keyword)))))

(defn send-message! [out message]
  (let [json-str (json/generate-string message)
        length   (count json-str)]
    (log/info "Send message:" message)
    (doto out
      (.write (str "Content-Length: " length "\r\n\r\n" json-str))
      (.flush))))

(defn start-server []
  (let [in (BufferedReader/new *in*)
        out (BufferedWriter/new *out*)
        input-chan (chan)]
    (go (loop []
          (let [message (read-message! in)]
            (when message
              (>! input-chan message)))
          (recur)))
    {:in input-chan :out out}))

