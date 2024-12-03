(ns aoc.y2024.day-3
  (:require [aoc.common :as common]
            [clojure.string :as string]))

(def test-file "problems/2024/day-3-1-t.txt")
(def test-file-2 "problems/2024/day-3-2-t.txt")
(def filename "problems/2024/day-3-1.txt")

(def r #"mul\(([0-9]{1,3}),([0-9]{1,3})\)")

(defn load-data
  ([] (load-data filename))
  ([filename]
   (->> filename
        common/read-input
        (apply str))))

(defn problem-one
  ([] (problem-one filename))
  ([filename]
   (->> filename
        load-data
        (re-seq r)
        (map rest)
        (map (fn [m] (map #(Integer/parseInt %) m)))
        (map #(* (first %) (second %)))
        (apply +))))

(defn problem-two
  ([] (problem-two filename))
  ([filename]
   (as-> filename i
         (load-data i)
         (string/split i #"do\(\)")
         (map #(first (string/split % #"don't\(\)")) i)
         (apply str i)
         (re-seq r i)
         (map rest i)
         (map (fn [m] (map #(Integer/parseInt %) m)) i)
         (map #(* (first %) (second %)) i)
         (apply + i))))
