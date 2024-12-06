(ns aoc.y2024.day-6
  (:require [aoc.common :as common]
            [clojure.set :as set]
            [clojure.string :as string]))

(def test-file "problems/2024/day-6-1-t.txt")
(def filename "problems/2024/day-6-1.txt")


(defn find-obstructions [x y [curr & rest] obs]
  (cond
    (nil? curr) obs
    (= \# curr) (recur (inc x) y rest (conj obs [x y]))
    :default (recur (inc x) y rest obs)))

(defn find-guard-pos [input]
  (->> (map-indexed (fn [y row]
                      [(string/index-of row \^) y]) input)
       (filter #(some? (first %)))
       first))

(defn load-data
  ([] (load-data filename))
  ([filename]
   (let [input (common/read-input filename)
         row-obs (map-indexed (fn [i row] (find-obstructions 0 i row #{})) input)
         obs (apply set/union row-obs)
         size (count input)
         guard (find-guard-pos input)]
     [size obs guard])))

(defn next-pos [x y dir]
  (cond
    (= :north dir) [x (+ y -1)]
    (= :south dir) [x (+ y 1)]
    (= :east dir) [(+ x 1) y]
    (= :west dir) [(+ x -1) y]))

(defn next-dir [dir]
  (cond
    (= :north dir) :east
    (= :east dir) :south
    (= :south dir) :west
    (= :west dir) :north))

(defn in-bounds? [x y size]
  (and (< x size)
       (< y size)
       (<= 0 x)
       (<= 0 y)))

(defn add-to-visited [x y dir visited]
  (update visited [x y] (fn [curr]
                          (conj (or curr #{})
                                dir))))

(defn path [x y dir size obs visited]
  (let [visited? (contains? (visited [x y]) dir)
        [nx ny] (next-pos x y dir)]
    (cond
      (not (in-bounds? nx ny size))
      (add-to-visited x y dir visited)

      visited?
      :loop

      (contains? obs [nx ny])
      (recur x y (next-dir dir) size obs visited)

      :default
      (recur nx ny dir size obs (add-to-visited x y dir visited)))))

(defn problem-one
  ([] (problem-one filename))
  ([filename]
   (let [[size obs [x y]] (load-data filename)
         visited (path x y :north size obs {})]
     (count visited))))

(defn try-obs [x y size obs]
  (let [visited (keys (path x y :north size obs {}))]
    (map (fn [[ox oy]]
           (cond
             (= [x y] [ox oy]) :skipped-guard
             (contains? obs [ox oy]) :skipped
             (= :loop (path x y :north size (conj obs [ox oy]) {})) :loop
             :default :escape)) visited)))

(defn problem-two
  ([] (problem-two filename))
  ([filename]
   (let [[size obs [x y]] (load-data filename)
         tries (try-obs x y size obs)]
     (count (filter #(= :loop %) tries)))))

