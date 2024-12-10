(ns aoc.y2024.day-10
  (:require [aoc.common :as common]))

(def test-file "problems/2024/day-10-1-t.txt")
(def filename "problems/2024/day-10-1.txt")

(defn val-at [m x y]
  (get (get m y) x))

(defn neighbours [m [x y]]
  (let [v (val-at m x y)
        potential [[(inc x) y] [(dec x) y] [x (inc y)] [x (dec y)]]
        passable (filter (fn [[x y]]
                           (let [next-v (val-at m x y)]
                             (and next-v (= 1 (- next-v v))))) potential)]
    passable))

(defn find-starts [m]
  (->> m
       (map-indexed
         (fn [y row]
           (map-indexed
             (fn [x val] (when (= 0 val) [x y])) row)))
       (mapv #(filter some? %))
       (apply concat)))

(defn load-data
  ([] (load-data filename))
  ([filename]
   (let [m (->> filename
                  common/read-input
                  (mapv (fn [row] (mapv common/char->int row))))
         starts (find-starts m)]
     [m starts])))

(defn path [m [curr & rest] visited-peaks]
  (let [v (val-at m (first curr) (second curr))]
    (cond
      (nil? curr) visited-peaks
      (= 9 v) (recur m rest (conj visited-peaks curr))
      :default (recur m (concat (neighbours m curr) rest) visited-peaks))))

(defn problem-one
  ([] (problem-one filename))
  ([filename]
   (let [[m starts] (load-data filename)
         visited (map #(path m [%] #{}) starts)]
     (apply + (map count visited)))))

(defn problem-two
  ([] (problem-two filename))
  ([filename]
   (let [[m starts] (load-data filename)
         visited (map #(path m [%] []) starts)]
     (apply + (map count visited)))))