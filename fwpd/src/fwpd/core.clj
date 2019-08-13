(ns fwpd.core
  (:require [clojure.string :as string])
  (:gen-class))

(def filename "suspects.csv")
(def vamp-keys [:name :glitter-index])


(defn str->int
  [input]
  (Integer. input))


(def conversions {:name identity
                  :glitter-index str->int})

(defn convert
  [[key value]]
  [key ((conversions key) value)])

(defn label-info
  [tokens]
  (map (fn [key val] [key val]) vamp-keys tokens))


(defn parse
  [input-file]
  (let [lines (string/split-lines (slurp input-file))]
    (map #(string/split % #",") lines)))

(defn mapify
  [token-lines]
  (let [labeled-pairs (map label-info token-lines)
        raw-map (map #(apply conj {} %) labeled-pairs)]
    (map #(into {} (map convert %)) raw-map)))

(defn glitter-filter
  [minimum vamp-map]
  (filter #(> (:glitter-index %) minimum) vamp-map))
