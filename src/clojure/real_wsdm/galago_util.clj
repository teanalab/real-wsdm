(ns real-wsdm.galago-util
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.data.json :as json]
            [real-wsdm.print-util :refer :all])
  (:import org.lemurproject.galago.utility.Parameters
           org.lemurproject.galago.contrib.learning.LearnQueryParameters
           java.util.Arrays
           java.io.PrintStream
           org.lemurproject.galago.core.tools.App
           org.lemurproject.galago.core.parse.Document
           org.lemurproject.galago.core.parse.TagTokenizer
           org.lemurproject.galago.core.parse.stem.KrovetzStemmer))

(defn tokenize
  [text]
  (let [tokenizer (TagTokenizer.)
        document (Document.)]
    (set! (.text document) text)
    (.process tokenizer document)
    (vec (.terms document))))

(defn stem
  [token]
  (let [stemmer (KrovetzStemmer.)]
    (.stem stemmer token)))

(defn galago-query-operator
  [operator text]
  (str "#" operator "(" (->> text tokenize (str/join " ")) ")"))

(def wsdm-traversal (partial galago-query-operator "rwsdm"))

(defn eval-topics
  [queries-file]
  (map
    #(let [[_ number text] (re-matches #"(?s)<DOC (\d+)>\n(.*)" (str/trim %))]
      {:number number
       :text   (str/replace text #"\n" " ")})
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
  (-> (eval-topics-expanded-json query-expansion-function queries-file)
      json/write-str
      str))

(defn queries-json-tmp-file
  "create temprorary file with queries and return it"
  [& args]
  (let [queries-json-file (java.io.File/createTempFile "queries" ".json")]
    (spit queries-json-file (apply queries-json args))
    queries-json-file))

(defn queries-json-print
  "make queries file with required filename"
  [queries-file]
  (println (queries-json wsdm-traversal queries-file)))
;
(defn batch-search
  [print-stream query-expansion-function index-path queries-file]
  (let [queries-json-file (queries-json-tmp-file query-expansion-function
                                                 queries-file)]
    (App/run (into-array ["batch-search"
                          (str "--index=" index-path)
                          (str queries-json-file)
                          (-> "traversal-config.json" io/resource io/file str)])
             print-stream)
    (.delete queries-json-file)))

(defn batch-search-tmp-file
  [& args]
  (let [search-results (java.io.File/createTempFile "search-results" ".json")]
    (apply batch-search (PrintStream. search-results) args)
    search-results))

(defn batch-search-print
  [index-path queries-file]
  (batch-search System/out wsdm-traversal index-path queries-file))

(def features-config
  {:rwsdmFeatures
    [
      ;{:name    "1-const"
      ; :type    "const"
      ; :unigram true}
      {:name    "1-lntf"
       :type    "logtf"
       :unigram true}
      {:name    "1-lndf"
       :type    "logdf"
       :unigram true}
      ;{:name    "2-const"
      ; :type    "const"
      ; :unigram false
      ; :bigram  true}
      {:name    "2-lntf"
       :type    "logtf"
       :unigram false
       :bigram  true}
      {:name    "2-lndf"
       :type    "logdf"
       :unigram false
       :bigram  true}
      ;{:name    "3-const"
      ; :type    "const"
      ; :unigram false
      ; :bigram  false
      ; :trigram true}
      {:name    "3-lntf"
       :type    "logtf"
       :unigram false
       :bigram  false
       :trigram true}
      {:name    "3-lndf"
       :type    "logdf"
       :unigram false
       :bigram  false
       :trigram true}
      ]})

(def learnable-config
  {:learnableParameters
               [
                 ;{:name "1-const"
                 ; :max  3.0
                 ; :min  0.0}
                 {:name "1-lntf"
                  :max  3.0
                  :min  0.0}
                 {:name "1-lndf"
                  :max  3.0
                  :min  0.0}
                 ;{:name "2-const"
                 ; :max  3.0
                 ; :min  0.0}
                 {:name "2-lntf"
                  :max  3.0
                  :min  0.0}
                 {:name "2-lndf"
                  :max  3.0
                  :min  0.0}
                 ;{:name "3-const"
                 ; :max  3.0
                 ; :min  0.0}
                 {:name "3-lntf"
                  :max  3.0
                  :min  0.0}
                 {:name "3-lndf"
                  :max  3.0
                  :min  0.0}
                 ]
   :learner    "coord"})
;:initialParameters
; [(Parameters/parseString "{
; \"1-const\": 1,
; \"1-lntf\": 1,
; \"1-lndf\": 1,
; \"2-const\": 1,
; \"2-lntf\": 1,
; \"2-lndf\": 1}")]})
;\"3-const\": 1,
;\"3-lntf\": 1,
;\"3-lndf\": 1}")]})
;:learner "grid"
;:gridSize 3})
;:maxIterations 1000
;:restarts 50})

(def normalization-parameters
  (doto (Parameters/instance)
    (.set "mode" "sum")
    (.set "params" [
                     ;"1-const"
                     "1-lntf"
                     "1-lndf"
                     ;"2-const"
                     "2-lntf"
                     "2-lndf"
                     ;"3-const"
                     "3-lntf"
                     "3-lndf"
                     ])
    (.set "value" 1.0)))

;(defn string-tmp-file
;  [string]
;  (let [file (java.io.File/createTempFile "clojure" ".txt")]
;    (spit features-config-file string)
;    file))

(defn learn
  [index-path queries-file judgements-filepath]
  (let [parameters (doto (Parameters/instance)
                     ;(.set "verboseRWSDM" true)
                     (.set "index" index-path)
                     (.set "qrels" judgements-filepath)
                     (.copyFrom (Parameters/parseString (queries-json wsdm-traversal queries-file)))
                     (.copyFrom (Parameters/parseString (json/write-str features-config)))
                     (.copyFrom (Parameters/parseString (json/write-str learnable-config)))
                     (.set "normalization" [normalization-parameters])
                     (.copyFrom (-> "traversal-config.json" io/resource io/file str Parameters/parseFile)))]
    (.run (LearnQueryParameters.) parameters System/out)))

(defn mean-ap
  [query-expansion-operator index-path queries-file judgements-filepath]
  (let [query-expansion-function (partial galago-query-operator query-expansion-operator)
        batch-search-results (batch-search-tmp-file query-expansion-function
                                                    index-path
                                                    queries-file)
        average-precision (org.lemurproject.galago.core.eval.metric.AveragePrecision. "map")
        mean-average-precision (org.lemurproject.galago.core.eval.aggregate.Mean. "map" average-precision)
        query-set-results (org.lemurproject.galago.core.eval.QuerySetResults. (str batch-search-results))
        query-set-judgements (org.lemurproject.galago.core.eval.QuerySetJudgments. judgements-filepath false true)]
    (.evaluate mean-average-precision query-set-results query-set-judgements)))

(defn print-map
  [& args]
  (println (apply mean-ap args)))
