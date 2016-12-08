(ns aoc-2016.p4
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn parse-line
  [line]
  (let [[_ name sector checksum] (re-find #"^([a-z\-]+)(\d+)\[([a-z]+)\]$" line)]
    [name
     (Integer/parseInt sector)
     checksum]))

(defn count-letters
  [name]
  (reduce (fn [letter-map letter]
            (update letter-map
                    (str letter)
                    (fnil inc 0)))
          {}
          (seq name)))

(defn sort-letters
  [letter-counts]
  (->> letter-counts
       (into (sorted-map-by (fn [k1 k2]
                              (let [v1 (get letter-counts k1)
                                    v2 (get letter-counts k2)]
                                (if (= v1 v2)
                                  (compare k1 k2)
                                  (- v2 v1))))))
       keys))

(defn real-room?
  [name sector checksum]
  (let [letter-counts (count-letters (str/replace name "-" ""))
        sorted-letters (sort-letters letter-counts)]
    (= checksum (apply str (take 5 sorted-letters)))))

(defn char->int
  [char]
  (- (int char) 97))

(defn int->char
  [int]
  (char (+ int 97)))

(defn decypher
  [name sector]
  (let [decyphered-name (->> (seq name)
                             (map (fn [letter]
                                    (if (= letter \-)
                                      " "
                                      (-> (char->int letter)
                                          (+ sector)
                                          (mod 26)
                                          (int->char)))))
                             (apply str))]
    [(.trim decyphered-name) sector]))

(defn solve
  []
  (with-open [r (io/reader (io/resource "p4.txt"))]
    (let [real-rooms (->> (line-seq r)
                          (map parse-line)
                          (filter #(apply real-room? %)))]
      (->> real-rooms
           (map (fn [[name sector _]] (decypher name sector)))
           (filter (fn [[name sector]] (re-find #"northpole" name)))
           first
           second))))

#_(solve)

(def e1 "fubrjhqlf-edvnhw-dftxlvlwlrq-803[wjvzd]")
(def e2 "kzgwomvqk-rmttgjmiv-lmxizbumvb-902[zmnji]")
(def e3 "dkqjcbctfqwu-dwppa-fgukip-596[syiua]")
(def e4 "xjinphzm-bmvyz-ytz-gjbdnodxn-135[nzbdj]")

#_(->> (parse-line e4) (apply real-room?))
