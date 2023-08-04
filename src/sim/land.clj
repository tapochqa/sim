(ns sim.land
  (:require 
    [sim.helper :as helper]
    [clojure.math :as math]))

(def WATER "🟦")
(def LAND  "🟫")


(defn in-2d
  [coll x y]
  (nth (nth coll x) y))


(defn draw-land
  [{:keys [land h]}]
  
  (let [land
        (partition h land)
        
        simple
        (mapv
          (fn [row]
            (mapv
              (fn [cell]
                (if (= (:state cell) LAND)
                  LAND
                  WATER))
              row))
          land)
        
        stringified
        (map (fn [x] (reduce str x)) simple)]
    
    (mapv println stringified)))


(defn distance
  [a b]
  (abs (- b a)))


(defn growable?
  [{:keys [land]} {:keys [x y max-range state]}]
  
  (let [cross
        (filter
          (fn [c]
            (or
              (= (:x c) x)
              (= (:y c) y)
              (= (distance (:x c) x)
                 (distance (:y c) y))
              
              
              ))
          land)
        
        filled-neigbour?
        (fn [c]
            (and
              (or
                (= (distance (:x c) x) 1)
                (= (distance (:y c) y) 1))
              (= (:state c) LAND)))
        
        filled-neigbours
        (filter
          filled-neigbour?
          cross)
        
        any-neigbour
        (first filled-neigbours)
        
        no-neighbours
        (remove
          (fn [c] (= (:island c) (:island any-neigbour)))
          cross)
        
        interruptors
        (filter
          (fn [c]
            (and
              (and
                (<= (distance (:x c) x) max-range)
                (<= (distance (:y c) y) max-range))
              (= (:state c) LAND)))
          no-neighbours)]
    
    (if 
      (and
        (= state WATER)
        (some? any-neigbour)
        (= [] interruptors))
      (:island any-neigbour) false))
  
  )


(defn grow
  [land]
  (assoc land :land
    (map (fn [cell]
           (let [island (growable? land cell)]
             (if 
               island
               (assoc cell :state LAND
                           :island island)
             cell)))
      (:land land))))


(defn generate-land
  "generate a square land of :w width with islands and rivers of a width between :min and :max"
  [& {:keys [w islands min max]}]
  (let [h w 
        land
        (take (* w h) 
          (sequence 
            (map 
              (fn [_] 
                   {:max-range (helper/rand-in-range min max)
                    :state   WATER}))
            (range)))
        
        land
        (map-indexed 
          (fn [i x] 
            (if  
              (< i islands)
              
              (assoc x 
                :state LAND
                :island i)
              x))       
         land)
        
        
        land
        (shuffle land)
        
        land
        (map-indexed
          (fn [i x]
            (assoc x 
              :x (math/floor-mod i w)
              :y (math/floor-div i w)
              :index i))
          land)
        
        land
        {:w w
         :h h
         :islands islands
         :land land}]
    
    (last
     (take w
       (iterate (fn [l] (grow l)) land)))))


(comment
  
  (do
    (println)
    (println)
    (draw-land
      (generate-land 
        :w 20
        :islands 4
        :min 2 
        :max 3)))
  
  (concat [1 2] [3 4])
  (= [] ())
  (seq '(1)))