(ns real-wsdm.wiki-ngrams
  (:require [clojure.java.io :as io]
            [real-wsdm.galago-util :refer :all]
            [clojure.string :as str]))

(defn extract-ngrams
  [tokens]
  (concat
    (map list tokens)
    (partition 2 1 tokens)
    (partition 3 1 tokens)))

(defn add-ngram
  [ngram-count ngram]
  (let [old-value (get ngram-count ngram 0)]
    (assoc ngram-count ngram (inc old-value))))

(defn add-ngrams
  [ngram-count ngrams]
  (reduce
    add-ngram
    ngram-count
    ngrams))

(defn ngram-count
  [lines]
  (reduce
    add-ngrams
    {}
    (map #(->> % tokenize (map stem) extract-ngrams) lines)))

(defn titles
  [lines]
  (reduce
    add-ngram
    {}
    (filter #(<= (count %) 3) (map #(->> % tokenize (map stem)) lines))))

(defn extract-ngrams
  [wiki-titles-path]
  (with-open [rdr (io/reader wiki-titles-path)]
    (doseq [[ngram count] (ngram-count (line-seq rdr))]
      (println (str (str/join " " ngram) "\t" count)))))

(defn extract-titles
  [wiki-titles-path]
  (with-open [rdr (io/reader wiki-titles-path)]
    (doseq [[ngram _] (titles (line-seq rdr))]
      (println (str (str/join " " ngram) "\t" 100)))))
