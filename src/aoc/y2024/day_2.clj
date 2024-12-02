(ns aoc.y2024.day-2
  (:require [aoc.common :as common]
            [clojure.string :as string]))

(def test-file "problems/2024/day-2-1-t.txt")
(def filename "problems/2024/day-2-1.txt")

(defn load-data
  ([] (load-data filename))
  ([filename]
   (->> filename
        common/read-input
        (map #(string/split % #" "))
        (map #(map (fn [v] (Integer/parseInt v)) %))
        (map vec))))

(defn state [diff]
  (cond
    (pos? diff) :increasing
    (neg? diff) :decreasing
    (= 0 diff) :stable))

(defn safe? [prev-state last [curr & rest] all]
  (if (nil? curr)
    [true all]
    (let [diff (- curr last)
          state (state diff)]
      (cond
        (= :stable state) [false all]
        (< 3 (abs diff)) [false all]
        (= :start prev-state) (recur state curr rest all)
        (not= prev-state state) [false all]
        :default (recur state curr rest all)))))

(defn problem-one
  ([] (problem-one filename))
  ([filename]
   (->> filename
        load-data
        (map (fn [report] (safe? :start (first report) (rest report) report)))
        (filter identity)
        count)))

(defn retry-safe? [report]
  (let [attempts (map (fn [i]
                        (let [report (concat (subvec report 0 i)
                                             (subvec report (inc i)))]
                          (safe? :start (first report) (rest report) report)))
                      (range (count report)))]
    (or (some #(first %) attempts) false)))

(defn problem-two
  ([] (problem-two filename))
  ([filename]
   (->> filename
        load-data
        (map (fn [report] (safe? :start (first report) (rest report) report)))
        (map (fn [[safe? report]] (if safe? true (retry-safe? report))))
        (filter identity)
        count)))