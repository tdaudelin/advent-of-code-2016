(ns aoc-2016.p2
  (:require [clojure.java.io :as io]))

(defn move-vertical
  [y]
  (fn [start]
    (let [val (+ start y)]
      (if (or (> val 9)
              (< val 1))
        start
        val))))

(def move-up (move-vertical -3))
(def move-down (move-vertical 3))

(defn move-left
  [start]
  (if (not= (mod start 3) 1)
    (- start 1)
    start))

(defn move-right
  [start]
  (if (not= (mod start 3) 0)
    (+ start 1)
    start))

(def move-map
  {:L move-left
   :R move-right
   :U move-up
   :D move-down})

(defn parse-line
  [line]
  (when line
    (->> line seq (map #(-> % str keyword)))))

(defn reduce-line
  [key-seq line]
  (reduce (fn [start instruction]
            ((move-map instruction) start))
          (last key-seq)
          (parse-line line)))

(defn solve
  []
  (with-open [r (io/reader (io/resource "p2.txt"))]
    (->> (line-seq r)
         (reduce #(conj %1 (reduce-line %1 %2)) [5])
         rest
         (apply str))))
