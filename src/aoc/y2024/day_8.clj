(ns aoc.y2024.day-8
  (:require [aoc.common :as common]
            [clojure.set :as set]))

(def test-file "problems/2024/day-8-1-t.txt")
(def filename "problems/2024/day-8-1.txt")

(defn find-antennas-row [x y [curr & rest] ants]
  (cond
    (nil? curr) ants
    (not= \. curr) (recur (inc x) y rest
                          (update ants curr
                                  (fn [v]
                                    (conj (or v #{}) [x y]))))
    :default (recur (inc x) y rest ants)))

(defn find-antennas [y [curr & rest] ants]
  (if (nil? curr)
    ants
    (recur (inc y) rest (find-antennas-row 0 y curr ants))))

(defn load-data
  ([] (load-data filename))
  ([filename]
   (let [input (common/read-input filename)
         frequencies (find-antennas 0 input {})
         antennas (apply set/union (vals frequencies))]
     [(count input) frequencies antennas])))

(defn in-bounds? [size [x y]]
  (and (<= 0 x)
       (<= 0 y)
       (< x size)
       (< y size)))

(defn anti-nodes [_ [x1 y1] [x2 y2]]
  (if (and (= x1 x2) (= y1 y2))
    []
    (let [dx (- x1 x2)
          dy (- y1 y2)]
      [[(+ x1 dx) (+ y1 dy)]
       [(- x2 dx) (- y2 dy)]])))

(defn pairs [size ants f]
  (->> (for [a ants
             b ants]
         (f size a b))
       (apply concat)
       (filter #(in-bounds? size %))
       (into #{})))

(defn problem-one
  ([] (problem-one filename))
  ([filename]
   (let [[size frequencies] (load-data filename)
         freq-anti-nodes (map (fn [[k v]] (pairs size v anti-nodes)) frequencies)
         anti-nodes (apply set/union freq-anti-nodes)]
     (count anti-nodes))))

(defn project-anti-nodes [size x y dx dy]
  (if (not (in-bounds? size [x y]))
    []
    (conj (project-anti-nodes size (+ x dx) (+ y dy) dx dy) [x y])))

(defn anti-nodes-p2 [size [x1 y1] [x2 y2]]
  (if (and (= x1 x2) (= y1 y2))
    []
    (let [dx (- x1 x2)
          dy (- y1 y2)]
      (concat (project-anti-nodes size x1 y1 dx dy)
              (project-anti-nodes size x2 y2 (* -1 dx) (* -1 dy))))))

(defn problem-two
  ([] (problem-two filename))
  ([filename]
   (let [[size frequencies] (load-data filename)
         freq-anti-nodes (map (fn [[k v]] (pairs size v anti-nodes-p2)) frequencies)
         anti-nodes (apply set/union freq-anti-nodes)]
     (count anti-nodes))))
