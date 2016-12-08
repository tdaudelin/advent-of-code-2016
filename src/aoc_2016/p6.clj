(ns aoc-2016.p6
  (:require [clojure.java.io :as io]))

(def example ["eedadn"
              "drvtee"
              "eandsr"
              "raavrd"
              "atevrs"
              "tsrnev"
              "sdttsa"
              "rasrtv"
              "nssdts"
              "ntnada"
              "svetve"
              "tesnvt"
              "vntsnd"
              "vrdear"
              "dvrsen"
              "enarar"])

(defn count-chars
  [column]
  (reduce (fn [counts char]
            (update counts char (fnil inc 0)))
          {} column))
#_(count-chars [\a \b \c \a \a \b])

(defn error-correct-1
  [column]
  (let [char-counts (count-chars column)]
    (->> char-counts
         (reduce (fn [[_ curr-v :as curr] [_ next-v :as next]]
                   (if (> next-v curr-v)
                     next
                     curr))
                 [nil 0])
         first)))

(defn sort-chars
  "Sorts the keys in char-counts in ascending order by their values"
  [char-counts]
  (->> char-counts
       (into (sorted-map-by (fn [k1 k2]
                              (let [v1 (get char-counts k1)
                                    v2 (get char-counts k2)]
                                (- v1 v2)))))
       keys))
#_(sort-chars {\a 1 \b 2 \c 3})

(defn error-correct-2
  [column]
  (let [char-counts (count-chars column)
        sorted-chars (sort-chars char-counts)]
    (first sorted-chars)))

(defn unjam
  [correction-fn messages]
  (let [row-count (count messages)]
    (->> messages
         (apply interleave)
         (partition row-count)
         (map correction-fn)
         (apply str))))

#_(unjam error-correct-1 example)
#_(unjam error-correct-2 example)

(defn solve
  []
  (with-open [r (io/reader (io/resource "p6.txt"))]
    (unjam error-correct-2 (line-seq r))))
