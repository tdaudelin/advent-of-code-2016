(ns aoc-2016.p2.2
  (:require [clojure.java.io :as io]
            [aoc-2016.p2 :refer [parse-line]]))

(def keypad '[[_ _ 1 _ _]
              [_ 2 3 4 _]
              [5 6 7 8 9]
              [_ A B C _]
              [_ _ D _ _]])

(defn key
  [coords]
  (get-in keypad coords))

(defn move
  [dx dy]
  (fn [[x y :as start]]
    (let [nx (+ x dx)
          ny (+ y dy)
          key (key [nx ny])]
      (if (and key (not= key '_))
        [nx ny]
        start))))

(def move-map
  {:L (move 0 -1)
   :R (move 0 1)
   :U (move -1 0)
   :D (move 1 0)})

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
         (reduce (fn [key-seq line]
                   (conj key-seq (reduce-line key-seq line)))
                 [[2 0]])
         rest ;; strip off starting location
         (map key)
         (apply str))))
#_(solve)
