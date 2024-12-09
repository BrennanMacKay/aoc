(ns aoc.y2024.day-9
  (:require [aoc.common :as common]))

(def test-file "problems/2024/day-9-1-t.txt")
(def filename "problems/2024/day-9-1.txt")

(defn project-val [id size storage]
  (if (= size 0)
    storage
    (recur id (dec size) (conj storage id))))

(defn build-storage [step idx [curr & rest] files free storage]
  (let [id (/ step 2)
        curr (and curr (- (int curr) 48))]
    (cond
      (nil? curr) [storage files free]
      (even? step) (recur (inc step) (+ idx curr) rest
                       (conj files {:id id :pos idx :size curr}) free
                       (project-val id curr storage))
      :default (recur (inc step) (+ idx curr) rest
                      files (conj free {:id (int id) :pos idx :size curr})
                      (project-val nil curr storage)))))

(defn load-data
  ([] (load-data filename))
  ([filename]
   (let [input (first (common/read-input filename))]
     (build-storage 0 0 input [] [] []))))

(defn merge-storage [start end storage]
  (let [id (storage end)]
    (cond
      (<= end start) storage
      (nil? (storage end)) (recur start (dec end) storage)
      (nil? (storage start)) (recur (inc start) (dec end)
                                    (-> storage
                                        (assoc start id)
                                        (assoc end nil)))
      :default (recur (inc start) end storage))))

(defn checksum [storage]
  (apply + (map-indexed (fn [i v]
                          (if (nil? v)
                            0
                            (* i v))) storage)))

(defn problem-one
  ([] (problem-one filename))
  ([filename]
   (let [storage (first (load-data filename))
         merged (merge-storage 0 (dec (count storage)) storage)]
     (checksum merged))))

(defn find-dest [file free]
  (first (filter (fn [f]
                   (and (< (:pos f) (:pos file))
                        (<= (:size file) (:size f))))
                 free)))

(defn move-storage [id size start storage]
  (if (= size 0)
    storage
    (recur id (dec size) (inc start) (assoc storage start id))))

(defn update-free [size dest free]
  (let [updated {:id (:id dest)
                 :pos (+ size (:pos dest))
                 :size (- (:size dest) size)}]
    (assoc free (:id dest) updated)))

(defn merge-blocks [[curr & rest] free storage]
  (let [dest (and curr (find-dest curr free))]
    (cond
      (nil? curr) storage
      (nil? dest) (recur rest free storage)
      :default (recur rest
                      (update-free (:size curr) dest free)
                      (->> storage
                           (move-storage (:id curr) (:size curr) (:pos dest))
                           (move-storage nil (:size curr) (:pos curr)))))))

(defn problem-two
  ([] (problem-two filename))
  ([filename]
   (let [[storage files free] (load-data filename)
         files (vec (reverse (sort-by :id files)))
         free (vec (sort-by :id free))
         merged (merge-blocks files free storage)]
     (checksum merged))))
