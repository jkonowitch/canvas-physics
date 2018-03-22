(ns canvas.core
    (:require [clojure.math.combinatorics :as combo]))

(enable-console-print!)

;; ----------------
;; World Boundaries

(def innerWidth (.-innerWidth js/window))

(def innerHeight (.-innerHeight js/window))

(defn x-bound [radius] (- innerWidth radius))

(defn y-bound [radius] (- innerHeight radius))

;; ----------------
;; 2D Physics

(defn gravity
  [circle]
  (update circle :dy + .5))

(defn magnitude
  "Returns the scalar magnitude of a 2-d vector."
  [v->]
  (->> v->
      (map #(.pow js/Math % 2))
      (reduce +)
      (.sqrt js/Math)))

(defn dot-product
  [& vs->]
  (reduce + (apply map * vs->)))

(defn one-d-velocity
  "Newtonian equation of elastic collisions in one dimension. Takes the velocity
  and mass of two objects and returns the resulting velocity for the first object."
  [v1 m1 v2 m2]
  (let [numerator (+ (* v1 (- m1 m2)) (* 2 m2 v2))
        denominator (+ m1 m2)]
    (/ numerator denominator)))

(defn collision-physics
  "Takes two intersecting particles and calculates the new velocity vector
  for each particle. Variables followed by -> are vectors.
  Algorithm adapted from http://vobarian.com/collisions/2dcollisions2.pdf"
  [[[i c1] [j c2]]]
  (let [c1->    (map c1 [:x :y])
        c2->    (map c2 [:x :y])
        vc1->   (map c1 [:dx :dy])
        vc2->   (map c2 [:dx :dy])
        ; normal
        nrml->  (map - c1-> c2->)
        ; unit normal
        un->    (map #(/ % (magnitude nrml->)) nrml->)
        ; unit tangent
        ut->    [(- (last un->)) (first un->)]
        vc1n    (dot-product un-> vc1->)
        vc2n    (dot-product un-> vc2->)
        vc1t    (dot-product ut-> vc1->)
        vc2t    (dot-product ut-> vc2->)
        vc1n'   (one-d-velocity vc1n (:radius c1) vc2n (:radius c2))
        vc2n'   (one-d-velocity vc2n (:radius c2) vc1n (:radius c1))
        vc1n->' (map (partial * vc1n') un->)
        vc2n->' (map (partial * vc2n') un->)
        vc1t->' (map (partial * vc1t) ut->)
        vc2t->' (map (partial * vc2t) ut->)
        vc1->'  (map + vc1n->' vc1t->')
        vc2->'  (map + vc2n->' vc2t->')
        c1'     (merge c1 (zipmap [:dx :dy] vc1->'))
        c2'     (merge c2 (zipmap [:dx :dy] vc2->'))]
    [[i c1'][j c2']]))

(defn bounce-off-walls
  [{y :y x :x radius :radius :as c}]
  (let [xb (x-bound radius) yb (y-bound radius)]
    (cond
       (< y radius) (-> c (assoc :y radius) (update :dy -))
       (< yb y)     (-> c (assoc :y yb) (update :dy -))
       (< x radius) (-> c (assoc :x radius) (update :dx -))
       (< xb x)     (-> c (assoc :x xb) (update :dx -))
       :else c)))

;; ----------------
;; Collision Detection

(defn move-back
  [{dx :dx dy :dy :as circle}]
  (-> circle
      (update :x - dx)
      (update :y - dy)))

(defn collision? [circles]
  (let [dx (apply - (map :x circles))
        dy (apply - (map :y circles))
        d  (.sqrt js/Math (+ (* dx dx) (* dy dy)))
        min-d (apply + (map :radius circles))]
    (< d min-d)))

(defn colliding-tuples
  [circles]
  (as-> circles c
    (map-indexed vector c)
    (combo/combinations c 2)
    (filter (comp collision? #(map second %)) c)))

(defn m-assoc
  [cs [i circle]]
  (assoc cs i circle))

(defn simulate-collisions
  "Takes circles and resolves any collisions."
  [circles]
  (if-let [needs-resolution (not-empty (colliding-tuples circles))]
    (->> needs-resolution
          (map (partial map #(update % 1 move-back)))
          (map collision-physics)
          (apply concat)
          (reduce m-assoc (vec circles)))
    circles))

;; ----------------
;; Action Functions

(defn move-forward
  [{dx :dx dy :dy :as circle}]
  (-> circle
      (update :x + dx)
      (update :y + dy)))

(defn step-world
  "Main function that moves the world forward a step."
  [circles]
  (->> circles
       (map move-forward)
       (map bounce-off-walls)
       (simulate-collisions)))

;; ----------------
;; Object Creation

(defn rand-betw [a b]
  (+ a (rand (- b a))))

(def big [30 60])

(def small [10 20])

(defn make-circle []
  (let [radius (apply rand-betw big)]
    {:x        (rand-betw radius (x-bound radius))
     :y        (rand-betw radius (y-bound radius))
     :dx       (rand-betw 1 8)
     :dy       (rand-betw 1 8)
     :radius   radius}))

(defn conflicts
 [c circles]
 (->> (combo/cartesian-product [c] circles)
      (filter collision?)))

(defn spawn-non-intersecting
  "Returns n circles that do not intersect."
  [n]
  (loop [circles []
         c (make-circle)]
    (cond
      (= n (count circles))             circles
      (not-empty (conflicts c circles)) (recur circles (make-circle))
      :else                             (recur (conj circles c) (make-circle)))))

;; ----------------
;; Side Effects

(def canvas (.querySelector js/document "canvas"))

(def ctx (.getContext canvas "2d"))

(set! (.-height canvas) innerHeight)

(set! (.-width canvas) innerWidth)

(def two-radians (* 2 (.-PI js/Math)))

(defn draw! [circle]
  (.beginPath ctx)
  (set! (.-strokeStyle ctx) "blue")
  (set! (.-fillStyle ctx) "purple")
  (.arc ctx (:x circle) (:y circle) (:radius circle) 0 two-radians)
  (.stroke ctx)
  (.fill ctx))

(def frame! (.-requestAnimationFrame js/window))

(defn animate! [circles]
  (.clearRect ctx 0 0 innerWidth innerHeight)
  (doseq [c circles] (draw! c))
  (frame! (partial animate! (step-world circles))))

(animate! (spawn-non-intersecting 15))

;; TODO
;; do broad phase - grid
