(ns aoc-2016.p5
  (:require [digest :refer [md5]]))

(def door-id "uqwqemis")

(defn door-reducer-1
  [password guess]
  (if (= 8 (count password))
    (reduced password)
    (let [hash (md5 guess)
          match (re-find #"^0{5}." hash)]
      (if match
        (conj password (last match))
        password))))

(defn door-reducer-2
  [password guess]
  (if (every? identity password)
    (reduced password)
    (let [hash (md5 guess)
          [_ index value] (re-find #"^0{5}([0-7])(.)" hash)]
      (if (and index (nil? (get password (Integer/parseInt index))))
        (assoc password (Integer/parseInt index) value)
        password))))

(defn crack-password
  [door-reducer door-id password-init]
  (->> (map #(str door-id %) (range))
       (reduce door-reducer password-init)
       (apply str)))
#_(crack-password door-reducer-1 "abc" [])
#_(door-reducer-2 (vec (repeat 8 nil)) "abc3231929")
#_(crack-password door-reducer-2 "abc" (vec (repeat 8 nil)))


(defn solve
  []
  (crack-password door-reducer-2 door-id (vec (repeat 8 nil))))
