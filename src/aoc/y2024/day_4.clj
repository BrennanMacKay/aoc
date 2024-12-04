(ns aoc.y2024.day-4
  (:require [aoc.common :as common]))

(def test-file "problems/2024/day-4-1-t.txt")
(def filename "problems/2024/day-4-1.txt")

(def directions [[0 1]
                 [1 0]
                 [1 1]
                 [0 -1]
                 [-1 0]
                 [-1 -1]
                 [-1 1]
                 [1 -1]])

(defn char-at [grid x y]
  (get (get grid y) x))

(defn load-data
  ([] (load-data filename))
  ([filename]
   (->> filename
        common/read-input)))

(defn check-direction [grid x y [curr & rest] dx dy]
  (cond
    (nil? curr) true
    (not= (char-at grid x y) curr) false
    :default (recur grid (+ x dx) (+ y dy) rest dx dy)))

(defn start-from [grid x y word]
  (->> (map (fn [[dx dy]]
             (check-direction grid x y word dx dy))
           directions)
      (filter identity)
      count))

(defn problem-one
  ([] (problem-one filename))
  ([filename]
   (let [grid (load-data filename)]
     (apply + (for [x (range (count (first grid)))
                    y (range (count grid))]
                (start-from grid x y "XMAS"))))))

(defn x? [grid x y]
  (let [char-at (partial char-at grid)
        middle (char-at x y)
        forward [(char-at (+ x -1) (+ y 1)) middle (char-at (+ x 1) (+ y -1))]
        back [(char-at (+ x -1) (+ y -1)) middle (char-at (+ x 1) (+ y 1))]
        mas? (fn [word]
               (or (= [\M \A \S] word)
                   (= [\S \A \M] word)))]
    (cond
      (not= \A middle) 0
      (and (mas? forward) (mas? back)) 1
      :default 0)))

(defn problem-two
  ([] (problem-two filename))
  ([filename]
   (let [grid (load-data filename)]
     (apply + (for [x (range (count (first grid)))
                    y (range (count grid))]
                (x? grid x y))))))
