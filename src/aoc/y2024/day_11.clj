(ns aoc.y2024.day-11
  (:require [aoc.common :as common]
            [clojure.string :as string]))

(def test-file "problems/2024/day-11-1-t.txt")
(def filename "problems/2024/day-11-1.txt")


(defn load-data
  ([filename]
   (as-> filename v
         (common/read-input v)
         (first v)
         (string/split v #" ")
         (map #(Long/parseLong %) v)
         (frequencies v))))

(defn step [[curr & rest] next]
  (if (nil? curr)
    next
    (let [[val num] curr
          as-string (str val)
          length (count as-string)]
      (cond
        (= 0 val)
        (recur rest (update next 1 #(+ (or % 0) num)))

        (even? length)
        (recur rest
               (-> next
                   (update (Long/parseLong (subs as-string 0 (/ length 2)))
                           #(+ (or % 0) num))
                   (update (Long/parseLong (subs as-string (/ length 2)))
                           #(+ (or % 0) num))))

        :default
        (recur rest (update next (* 2024 val) #(+ (or % 0) num)))))))

(defn steps [rem stones]
  (if (= 0 rem)
    stones
    (recur (dec rem) (step stones {}))))

(defn problem-one
  ([] (problem-one filename 25))
  ([filename num-steps]
   (let [stones (load-data filename)
         after (steps num-steps stones)]
     (apply + (vals after)))))

(defn problem-two []
  (problem-one filename 75))

