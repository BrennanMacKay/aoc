(ns aoc.y2024.day-1
  (:require [aoc.common :as common]
            [clojure.string :as string]))

(def test-file "problems/2024/day-1-1-t.txt")
(def filename "problems/2024/day-1-1.txt")

(defn build-up-lists [l1 l2 [curr & rest]]
  (if (nil? curr)
    [l1 l2]
    (recur (conj l1 (Integer/parseInt (first curr)))
           (conj l2 (Integer/parseInt (second curr)))
           rest)))

(defn diff [diffs [[l1c & l1r] [l2c & l2r]]]
  (if (nil? l1c)
    diffs
    (recur (conj diffs (abs (- l1c l2c))) [l1r l2r])))

(defn load-data
  ([] (load-data filename))
  ([filename]
   (->> filename
        common/read-input
        (map #(string/split % #"   "))
        (build-up-lists [] []))))

(defn problem-one
  ([] (problem-one filename))
  ([filename]
   (->> filename
        load-data
        (map sort)
        (diff [])
        (apply +))))

(defn freq-map [[l1 l2]]
  [l1 (frequencies l2)])

(defn sim [sims [[curr & rest] freq]]
  (if (nil? curr)
    sims
    (do
      (recur (conj sims (* curr (freq curr 0))) [rest freq]))))

(defn problem-two
  ([] (problem-two filename))
  ([filename]
   (->> filename
        load-data
        freq-map
        (sim [])
        (apply +))))