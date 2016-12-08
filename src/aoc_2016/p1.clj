(ns aoc-2016.p1
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(defn parse-input
  [input]
  (-> input
      str/trim
      (str/split #", ")
      (->> (map (fn [instruction]
                  (let [[turn & steps] (seq instruction)]
                    [(keyword (str turn)) (Integer/parseInt (apply str steps))]))))))

(def turns
  {:N {:R :E :L :W}
   :E {:R :S :L :N}
   :S {:R :W :L :E}
   :W {:R :N :L :S}})

(defn new-pos
  [heading mag x y]
  (case heading
    :N [x (+ y mag)]
    :E [(+ x mag) y]
    :S [x (- y mag)]
    :W [(- x mag) y]))

(defn visits
  [heading mag x y]
  (case heading
    :N (map #(vector x (+ y (inc %))) (range mag))
    :E (map #(vector (+ x (inc %)) y) (range mag))
    :S (map #(vector x (+ y (dec %))) (range 0 (* -1 mag) -1))
    :W (map #(vector (+ x (dec %)) y) (range 0 (* -1 mag) -1))))
#_ (visits :E 8 0 0)
#_ (visits :N 5 2 3)
#_ (visits :S 5 2 3)
#_ (visits :W 8 0 0)


(defn walk
  [instructions]
  (let [step (fn [acc instruction]
               (let [mag (second instruction)
                     heading (get-in turns [(:heading acc) (first instruction)])
                     [x y] (new-pos heading mag (:x acc) (:y acc))
                     new-visits (visits heading mag (:x acc) (:y acc))]
                 (or (some identity
                           (map (fn [visit]
                                  (when (contains? (:visited acc) visit)
                                    (reduced {:x (first visit) :y (second visit)})))
                                new-visits))
                     {:heading heading
                      :visited (apply conj (:visited acc) new-visits)
                      :x x
                      :y y})))]
    (reduce step {:heading :N :visited #{} :x 0 :y 0} instructions)))
#_(walk (parse-input "R8, R4, R4"))

(defn solve
  []
  (let [input (-> "p1.txt"
                  io/resource
                  slurp
                  parse-input)
        {:keys [x y]} (walk input)]
    (+ (Math/abs x) (Math/abs y))))

#_(solve)
