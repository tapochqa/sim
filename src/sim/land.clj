(ns sim.land
  (:require 
    [sim.helper :as helper]
    [clojure.math :as math]))


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
                (if (= (:state cell) 1)
                  1
                  0))
              row))
          land)
        
        stringified
        (map (fn [x] (reduce str x)) simple)]
    
    (mapv println stringified)))


(defn distance
  [a b]
  (abs (- a b))
  )


(defn growable?
  [{:keys [land]} {:keys [x y max-range state]}]
  
  (let [cross
        (filter
          (fn [c]
            
            (or
              (= (:x c) x)
              (= (:y c) y)))
          land)
        
        filled-neigbour?
        (fn [c]
            (and
              (= (distance (:x c) x) 1)
              (= (distance (:y c) y) 1)
              (= (:state c) 1)))
        
        filled-neigbours
        (filter
          filled-neigbour?
          cross)
        
        no-neighbours
        (remove
          filled-neigbour?
          cross)
        
        interruptors
        (filter
          (fn [c]
            (and
              (or
                (< (distance (:x c) x) max-range)
                (< (distance (:y c) y) max-range))
              (= (:state c) 1)))
          no-neighbours)]
    
    (if 
      (and
        (= state 0)
        (seq filled-neigbours)
        (= [] interruptors))
      true false))
  
  )


(defn grow
  [land]
  
  (assoc land :land
    (map (fn [cell]
           (if 
             (growable? cell land)
             (assoc cell :state 1)
             cell))
      (:land land))))


(let [w 10
      h 10
      islands 2
      
      
      land
      (take (* w h) 
        (sequence 
          (map-indexed 
            (fn [i x] 
                 {:max-range (helper/rand-in-range 1 4)
                  :state   0}))
          (range)))
      
      land
      (map-indexed 
        (fn [i x] 
          (if  
            (< i islands)
            
            (assoc x 
              :state 1
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
       :land land}
      
      
      ]
  
  (println)
  (draw-land (grow land))
  (grow land))


(comment
  (concat [1 2] [3 4])
  (= [] ())
  (seq '(1)))