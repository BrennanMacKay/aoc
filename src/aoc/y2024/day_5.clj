(ns aoc.y2024.day-5
  (:require [aoc.common :as common]
            [clojure.string :as string]))

(def testfile-rules "problems/2024/day-5-1-t-rules.txt")
(def filename-rules "problems/2024/day-5-1-rules.txt")

(def testfile-updates "problems/2024/day-5-1-t-updates.txt")
(def filename-updates "problems/2024/day-5-1-updates.txt")

(defn load-rules
  [filename]
  (->> filename
       common/read-input
       (map (fn [rule] (vec (map #(Integer/parseInt %) (string/split rule #"\|")))))
       (group-by first)
       (map (fn [[k v]] [k (into #{} (map #(second %) v))]))
       (into {})))

(defn load-updates
  [filename]
  (->> filename
       common/read-input
       (map (fn [update] (map #(Integer/parseInt %) (string/split update #","))))))

(defn sort-by-rules [rules update]
  (let [rule-cmp (fn [a b]
                   (cond
                     (contains? (rules a) b) -1
                     (contains? (rules b) a) 1
                     :default 0))]
    (sort rule-cmp update)))

(defn middle [coll]
  (nth coll (quot (count coll) 2)))

(defn problem-one
  ([] (problem-one filename-rules filename-updates))
  ([rules-file updates-file]
   (let [rules (load-rules rules-file)
         updates (load-updates updates-file)
         correct (filter (fn [update] (= update (sort-by-rules rules update))) updates)]
     (->> correct
          (map middle)
          (apply +)))))

(defn problem-two
  ([] (problem-two filename-rules filename-updates))
  ([rules-file updates-file]
   (let [rules (load-rules rules-file)
         updates (load-updates updates-file)
         incorrect (filter (fn [update] (not= update (sort-by-rules rules update))) updates)
         ;; sorting twice is kind of silly but :shrug:
         sorted (map #(sort-by-rules rules %) incorrect)]
     (->> sorted
          (map middle)
          (apply +)))))

