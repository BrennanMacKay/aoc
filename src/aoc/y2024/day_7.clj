(ns aoc.y2024.day-7
  (:require [aoc.common :as common]
            [clojure.string :as string]))

(def test-file "problems/2024/day-7-1-t.txt")
(def filename "problems/2024/day-7-1.txt")


(defn parse-line [line]
  (let [[result-raw parts] (string/split line #":")
        result (Long/parseLong result-raw)
        parts (map #(Long/parseLong %) (string/split (string/trim parts) #" "))]
    [result parts]))

(defn load-data
  ([] (load-data filename))
  ([filename]
   (->> filename
        common/read-input
        (map parse-line))))

(defn correct? [result [curr & rest] v]
  (cond
    (< result v) false
    (and (nil? curr)
         (= result v)) true
    (nil? curr) false
    :default (or (correct? result rest (+ v curr))
                 (correct? result rest (* v curr)))))

(defn problem-one
  ([] (problem-one filename))
  ([filename]
   (->> filename
        load-data
        (map (fn [[result parts]]
               (when (correct? result parts 0)
                 result)))
        (filter some?)
        (apply +))))

(defn concat-long [a b]
  (Long/parseLong (str a b)))

(defn correct-concat? [result [curr & rest] v]
  (cond
    (< result v) false
    (and (nil? curr)
         (= result v)) true
    (nil? curr) false
    :default (or (correct-concat? result rest (+ v curr))
                 (correct-concat? result rest (* v curr))
                 (correct-concat? result rest (concat-long v curr)))))

(defn problem-two
  ([] (problem-two filename))
  ([filename]
   (->> filename
        load-data
        (map (fn [[result parts]]
               (when (correct-concat? result parts 0)
                 result)))
        (filter some?)
        (apply +))))

