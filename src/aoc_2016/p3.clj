(ns aoc-2016.p3
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.set :as set]))

(defn triangle?
  [sides]
  (let [[s1 s2 longest] (sort sides)]
    (> (+ s1 s2) longest)))

#_(triangle? [1 2 3])
#_(triangle? [2 3 4])

(defn parse-line
  [line]
  (-> (.trim line)
      (str/split #"\s+")
      (->> (map #(Integer/parseInt %)))))

#_(parse-line "   5  43    83")

(defn solve
  []
  (with-open [r (io/reader (io/resource "p3.txt"))]
    (->> (line-seq r)
         (map #(parse-line %))
         (apply interleave)
         (partition 3)
         (map (fn [triangle-spec] (if (triangle? triangle-spec) 1 0)))
         (reduce +))))

#_(solve)
