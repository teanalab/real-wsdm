(ns real-wsdm.galago-util
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.data.json :as json]
            [real-wsdm.print-util :refer :all])
  (:import java.io.PrintStream
           org.lemurproject.galago.core.tools.App))

(defn remove-punctuation
  [text]
  (str/replace text #"[.,]" " "))

(defn galago-query-operator
  [operator text]
  (str "#" operator "(" (remove-punctuation text) ")"))

(def default-expansion-function (partial galago-query-operator "rwsdm"))

(defn eval-topics
  [queries-file]
  (map
    #(let [[_ number text] (re-matches #"(?s)<DOC (\d+)>\n(.*)" (str/trim %))]
      {:number number
       :text (str/replace text #"\n" " ")})
    (-> queries-file
        slurp
        (str/split #"\n</DOC>\n"))))

(defn eval-topics-expanded-json
  [query-expansion-function queries-file]
  {:queries (map #(hash-map :number (:number %)
                            :text (query-expansion-function (:text %)))
                 (eval-topics queries-file))})

(defn queries-json
  "constructs json string for queries"
  [query-expansion-function queries-file]
  (json/write-str (eval-topics-expanded-json query-expansion-function
                                             queries-file)))

(defn queries-json-tmp-file
  "create temprorary file with queries and return it"
  [query-expansion-function queries-file]
  (let [queries-json-file (java.io.File/createTempFile "queries" ".json")]
    (spit queries-json-file (queries-json query-expansion-function queries-file))
    queries-json-file))

(defn queries-json-print
  "make queries file with required filename"
  [queries-file]
  (println (queries-json default-expansion-function queries-file)))

(defn batch-search
  [query-expansion-function index-path queries-file print-stream]
  (let [queries-json-file (queries-json-tmp-file query-expansion-function
                                                 queries-file)]
    (App/run (into-array ["batch-search"
                          (str "--index=" index-path)
                          (str queries-json-file)
                          (-> "traversal-config.json" io/resource io/file str)])
             print-stream)
    (.delete queries-json-file)))

(defn batch-search-tmp-file
  [query-expansion-function index-path queries-file]
  (let [search-results (java.io.File/createTempFile "search-results" ".json")]
    (batch-search query-expansion-function
                  index-path
                  queries-file
                  (PrintStream. search-results))
    search-results))

(defn batch-search-print
  [index-path queries-file]
  (batch-search default-expansion-function index-path queries-file System/out))

(defn mean-ap
  [query-expansion-function index-path queries-file judgements-filepath]
  (let [batch-search-results (batch-search-tmp-file query-expansion-function
                                                    index-path
                                                    queries-file)
        average-precision (org.lemurproject.galago.core.eval.metric.AveragePrecision. "map")
        mean-average-precision (org.lemurproject.galago.core.eval.aggregate.Mean. "map" average-precision)
        query-set-results (org.lemurproject.galago.core.eval.QuerySetResults. (str batch-search-results))
        query-set-judgements (org.lemurproject.galago.core.eval.QuerySetJudgments. judgements-filepath false true)]
    (.evaluate mean-average-precision query-set-results query-set-judgements)))

