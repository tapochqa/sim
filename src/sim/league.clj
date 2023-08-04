(ns sim.league
  (:require 
    [sim.helper :as helper]
    [clojure.string :as str]))


(def NAMES "resources/male_names_rus.txt")
(def SURNAMES "resources/male_surnames_rus.txt")


(defn generate-name
  []
  (let [name 
        (helper/take-rand "resources/male_names_rus.txt")
        
        surname 
        (helper/take-rand "resources/male_surnames_rus.txt")]
    
    (format "%s %s" name surname)))


(defn generate-player
  []
  (let [points 
        (helper/rand-in-range 0 100)
        
        attack 
        (rand-int points)
        
        points' 
        (- points attack)
        
        defence
        (rand-int points')
        
        stamina
        (- points' defence)
        
        age
        (helper/rand-in-range 16 22)]
  
      {:name
       (helper/take-rand NAMES)
       
       :surname
       (helper/take-rand SURNAMES)
       
       :age
       age
       
       :peak-at
       (helper/rand-in-range (max 20 age) 30)
       
       :attack attack
       :defence defence
       :stamina stamina
       
       }))


(defn total-points
  [player]
  (+
    (:attack player)
    (:defence player)
    (:stamina player)))


(defn generate-team
  []
  (->>
     (map
      #(do % (generate-player))
      (range 10))
      
      (sort (fn [i j]
              (> 
                (total-points i) 
                (total-points j))))))


(defn minute
  [passed home away]
  
  (let [main-home 
        (take 5 home)
        
        main-away 
        (take 5 away)
        
        goal-home 
        (- (reduce + (map :attack home))
           (reduce + (map :defence away))
           (reduce (fn [a b] (int (/ (* 1000 passed) (+ a b 1)))) 
             (map :stamina home)))
        
        goal-away
        (- (reduce + (map :attack away))
           (reduce + (map :defence home))
           (reduce (fn [a b] (int (/ (* 1000 passed) (+ a b 1)))) 
             (map :stamina away)))
        
        
        thresh
        100
        
        dice-home
        (rand-int goal-home)
        
        dice-away
        (rand-int goal-away)
        
        goal-home?
        (if (> dice-home thresh)
          true
          false)
        
        goal-away?
        (if (> dice-away thresh)
          true
          false)
        
        scores
        (cond 
      
          (and 
            goal-home?
            goal-away?)
          {:home 1
           :away 1}
          
          (and 
            goal-home?
            (not goal-away?))
          {:home 1
           :away 0}
          
          (and 
            goal-away?
            (not goal-home?))
          {:home 0
           :away 1}
          
          :else
          {:home 0
           :away 0})]
    
    
    (into scores {:minute minute})))


(defn match
  [t1 t2]
  (let [match 
        (mapv (fn [i] (minute i t1 t2))
              (range 90))]
  
    (format "%s:%s"
      
      
      (->> match 
        (map :home)
        (reduce +))
      
      (->> match 
        (map :away)
        (reduce +)))))


(comment
  
    
  (let [t1 (generate-team)
        t2 (generate-team)
        t3 (generate-team)
        t4 (generate-team)]
    
    (println
      (format "\n t1-t2 %s \n t1-t3 %s \n t1-t4 %s \n t2-t3 %s \n t2-t4 %s \n t3-t4 %s"
      (match t1 t2)
      (match t1 t3)
      (match t1 t4)
      (match t2 t3)
      (match t2 t4)
      (match t3 t4))))
  
  

  
  (->> (generate-team)
    (take 5)
    (map :stamina)
    (reduce +))
  
  )
