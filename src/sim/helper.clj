(ns sim.helper
  (:require [clojure.string :as str]))


(defn rand-in-range
  [from to]
  (+ from (rand-int (- (inc to) from))))

(comment
  (rand-in-range 1 3))


(defn take-rand
  [filename]
  (-> filename
      slurp
      (str/split #"\n")
      rand-nth))