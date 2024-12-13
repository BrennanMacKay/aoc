(ns aoc.y2024.day-12
  (:require [aoc.common :as common]
            [clojure.set :as set]))

(def test-file "problems/2024/day-12-1-t.txt")
(def filename "problems/2024/day-12-1.txt")

(defn val-at [m x y]
  (get (get m y) x))

(defn inner-corners [x y m]
  (let [v (val-at m x y)
        vl (val-at m (dec x) y)
        vr (val-at m (inc x) y)
        vu (val-at m x (dec y))
        vd (val-at m x (inc y))
        dl? (and (= v vl) (= v vd) (not= v (val-at m (dec x) (inc y))))
        dr? (and (= v vr) (= v vd) (not= v (val-at m (inc x) (inc y))))
        ul? (and (= v vl) (= v vu) (not= v (val-at m (dec x) (dec y))))
        ur? (and (= v vr) (= v vu) (not= v (val-at m (inc x) (dec y))))]
    (filter identity [(and dl? :inner-corner)
                      (and dr? :inner-corner)
                      (and ul? :inner-corner)
                      (and ur? :inner-corner)])))

(defn neighbours [[x y] m known]
  (let [v (val-at m x y)
        potential [[:east [(inc x) y]]
                   [:west [(dec x) y]]
                   [:south [x (inc y)]]
                   [:north [x (dec y)]]]
        potential (filter #(not (contains? known (second %))) potential)
        same (map second (filter (fn [[_ [x y]]] (= v (val-at m x y))) potential))
        borders (mapv first (filter (fn [[_ [x y]]] (not= v (val-at m x y))) potential))
        inner (inner-corners x y m)
        borders (vec (concat borders inner))
        known (set/union known (set same))]
    [same known borders]))

(defn fill-region [[curr & rest] m visited region-known borders]
  (if (nil? curr)
    [visited region-known borders]
    (let [[same region curr-borders] (neighbours curr m region-known)]
      (recur (concat rest same)
             m
             (conj visited curr)
             (set/union region-known region)
             (if (< 0 (count curr-borders))
               (assoc borders curr curr-borders)
               borders)))))

(defn visit-location [x y m visited regions]
  (cond
    (<= (count m) x) (recur 0 (inc y) m visited regions)
    (<= (count m) y) regions
    (contains? visited [x y]) (recur (inc x) y m visited regions)
    :default
    (let [[visited region borders] (fill-region [[x y]] m visited #{[x y]} {})]
      (recur (inc x) y m visited (conj regions {:points region
                                                :borders borders
                                                :val (val-at m x y)})))))

(defn load-data
  ([] (load-data filename))
  ([filename]
   (let [m (->> filename
                common/read-input
                (mapv vec))
         regions (visit-location 0 0 m #{} [])]
     [m regions])))

;; This will be broken since I added the inner-corner
(defn problem-one
  ([] (problem-one filename))
  ([filename]
   (let [[_ regions] (load-data filename)]
     (apply + (map (fn [region]
                     (* (count (:points region))
                        (count (flatten (map second (:borders region)))))) regions)))))

(defn corners [region]
  (let [borders (:borders region)]
    (->> borders
         vals
         (filter (fn [t] (or (= :inner-corner (first t))
                             (< 1 (count t)))))
         (map-indexed (fn [i t] (let [inners (count (filter #(= :inner-corner %) t))
                                      outers (vec (sort (filter #(not= :inner-corner %) t)))
                                      outers (cond
                                               (and (= 2 (count outers))
                                                      (or
                                                        (= [:north :south] outers)
                                                        (= [:east :west] outers))) 0
                                               (= 4 (count outers)) 4
                                               :default (dec (count outers)))]
                        (+ inners (if (neg? outers) 0 outers)))))
         (apply +))))

(defn problem-two
  ([] (problem-two filename))
  ([filename]
   (let [[_ regions] (load-data filename)]
     (apply + (map (fn [region] (* (count (:points region)) (corners region))) regions)))))
