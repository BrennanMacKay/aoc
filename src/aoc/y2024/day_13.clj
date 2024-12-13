(ns aoc.y2024.day-13
  (:require [aoc.common :as common]))

(def test-file "problems/2024/day-13-1-t.txt")
(def filename "problems/2024/day-13-1.txt")


(defn read-instructions [rows instructions]
  (let [machine (take 4 rows)]
    (if (nil? (first machine))
      instructions
      (let [[a b prize] machine
            [_ ax ay] (re-matches #"Button A: X\+(\d+), Y\+(\d+)" a)
            [_ bx by] (re-matches #"Button B: X\+(\d+), Y\+(\d+)" b)
            [_ x y] (re-matches #"Prize: X=(\d+), Y=(\d+)" prize)]
        (recur (nthrest rows 4) (conj instructions
                                      {:ax (Integer/parseInt ax) :ay (Integer/parseInt ay)
                                       :bx (Integer/parseInt bx) :by (Integer/parseInt by)
                                       :x  (Integer/parseInt x) :y (Integer/parseInt y)}))))))

(defn load-data [filename]
  (read-instructions (common/read-input filename) []))

(defn cramers-rule [{:keys [ax ay bx by x y]}]
  (let [denom (- (* ax by) (* ay bx))
        a (/ (- (* x by) (* y bx)) denom)
        b (/ (- (* ax y) (* ay x)) denom)]
    (when (and (int? a) (int? b))
      [a b])))

(defn score [[a b]]
  (+ (* a 3) b))

(defn problem-one
  ([] (problem-one filename))
  ([filename]
   (let [instructions (load-data filename)]
     (->> instructions
          (map cramers-rule)
          (filter identity)
          (map score)
          (apply +)))))

(defn update-calibration [instruction]
  (-> instruction
      (update :x #(+ % 10000000000000))
      (update :y #(+ % 10000000000000))))

(defn problem-two
  ([] (problem-two filename))
  ([filename]
   (let [instructions (load-data filename)]
     (->> instructions
          (map update-calibration)
          (map cramers-rule)
          (filter identity)
          (map score)
          (apply +)))))
