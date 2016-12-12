(ns aoc-2016.p7
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(def example "wysextplwqpvipxdv[srzvtwbfzqtspxnethm]syqbzgtboxxzpwr[kljvjjkjyojzrstfgrw]obdhcczonzvbfby[svotajtpttohxsh]cooktbyumlpxostt")

(defn all-substrings
  "Creates a sequence of all substrings in str of the specified length"
  [str substr-len]
  (->> (range (- (count str) (dec substr-len)))
       (map #(take substr-len (drop % str)))))
#_(all-substrs "abcdefgh" 5)
;; => ((\a \b \c \d \e) (\b \c \d \e \f) (\c \d \e \f \g) (\d \e \f \g \h))

(defn substring-matcher
  [pred substr-len]
  (fn [str]
    (let [substrs (all-substrings str substr-len)]
      (filter pred substrs))))

(defn aba?
  [str]
  (and
   (= (count str) 3)
   (= (first str) (last str))
   (not= (first str) (second str))))

(def match-aba (substring-matcher aba? 3))
#_(match-aba "ghdkspiuvuvjklsdj")
;; => ((\u \v \u) (\v \u \v))
#_(match-aba "a;slkdjfitohvjl")
;; => ()

(defn abba?
  [str]
  (and
   (= (count str) 4)
   (= (first str) (last str))
   (= (second str) (nth str 2))
   (not= (first str) (second str))))

(def match-abba (substring-matcher abba? 4))
#_(match-abba "srzvtwbfzqqzzqtspxnethm")
;; => ((\z \q \q \z) (\q \z \z \q))
#_(match-abba "wysextplwqpvipxdv")
;; => ()

(defn parse-input
  [line]
  (let [hypernet-seqs (map second (re-seq #"\[([a-z]+)\]" line))
        supernet-seqs (str/split line #"\[[a-z]*\]")]
    [hypernet-seqs supernet-seqs]))
#_(parse-input example)
;; => [("srzvtwbfzqtspxnethm" "kljvjjkjyojzrstfgrw" "svotajtpttohxsh") ["wysextplwqpvipxdv" "syqbzgtboxxzpwr" "obdhcczonzvbfby" "cooktbyumlpxostt"]]

(defn supports-tls?
  [[hypernet-seqs supernet-seqs]]
  (let [any-match? (fn [seqs] (not-empty (mapcat match-abba seqs)))]
    (when-not (any-match? hypernet-seqs)
      (any-match? supernet-seqs))))

(defn solve_7-1
  []
  (with-open [r (io/reader (io/resource "p7.txt"))]
    (->> (line-seq r)
         (map parse-input)
         (keep supports-tls?)
         count)))

(defn supports-ssl?
  [[hypernet-seqs supernet-seqs]]
  (let [abas (set (mapcat match-aba supernet-seqs))]
    (->> abas
         (map (fn [[a b _]]
                (let [bab (str b a b)]
                  (->> (map #(.contains % bab) hypernet-seqs)
                       (some identity)))))
         (some identity))))
#_(supports-ssl? (parse-input example))
;; => nil

(defn solve_7-2
  []
  (with-open [r (io/reader (io/resource "p7.txt"))]
    (->> (line-seq r)
         (map parse-input)
         (keep supports-ssl?)
         count)))
