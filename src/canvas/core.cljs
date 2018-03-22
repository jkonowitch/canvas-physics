(ns canvas.core
    (:require [clojure.math.combinatorics :as combo]))

(enable-console-print!)

(def canvas (.querySelector js/document "canvas"))
(def ctx (.getContext canvas "2d"))

(def innerWidth (.-innerWidth js/window))
(def innerHeight (.-innerHeight js/window))

(set! (.-height canvas) innerHeight)
(set! (.-width canvas) innerWidth)

(def diameter 90)
(def radius (/ diameter 2))

(def x-bound (- innerWidth radius))
(def y-bound (- innerHeight radius))

(defn rand-betw [a b]
  (+ a (rand (- b a))))

(defn make-circle []
  {:x (rand-betw radius x-bound)
   :y (rand-betw radius y-bound)
   :dx (rand-betw 1 10)
   :dy (rand-betw 1 10)
   :radius (rand-betw 20 50)})

(defn resolve-wall-y
  [{y :y radius :radius :as circle}]
  (if-not (< radius y y-bound)
    (-> circle
        (update :dy -)
        (as-> c (update c :y + (:dy c))))
    circle))

(defn resolve-wall-x
  [{x :x radius :radius :as circle}]
  (if-not (< radius x x-bound)
    (-> circle
        (update :dx -)
        (as-> c (update c :x + (:dx c))))
    circle))

(defn gravity
  [circle]
  (update circle :dy + .5))

(defn move-forward
  [{dx :dx dy :dy :as circle}]
  (-> circle
      (update :x + dx)
      (update :y + dy)))

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

(def two-radians (* 2 (.-PI js/Math)))

(defn colliding-tuples [circles]
  (as-> circles c
    (map-indexed vector c)
    (combo/combinations c 2)
    (filter (comp collision? #(map second %)) c)))

(defn draw! [circle]
   (.beginPath ctx)
   (set! (.-strokeStyle ctx) "blue")
   (set! (.-fillStyle ctx) "purple")
   (.arc ctx (:x circle) (:y circle) (:radius circle) 0 two-radians)
   (.stroke ctx)
   (.fill ctx))

(defn magnitude
  "Returns the scalar magnitude of a 2-d vector."
  [v->]
  (->> v->
      (map #(.pow js/Math % 2))
      (reduce +)
      (.sqrt js/Math)))

(defn dot-product [& vs->]
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

(defn m-assoc
  [cs [i circle]]
  (assoc cs i circle))

(defn resolve-collisions
  [circles needs-resolution]
  (if (not-empty needs-resolution)
    (->> needs-resolution
          (map (partial map #(update % 1 move-back)))
          (map collision-physics)
          (apply concat)
          (reduce m-assoc (vec circles)))
    circles))

; (defn collisions
;   [circles]
;   (if-let [need-resolution (not-empty (colliding-tuples circles))]
;     (resolve-and-merge circles need-resolution)
;     circles))

(def move (comp resolve-wall-x
                resolve-wall-y
                move-forward))

(defn step-world
  [circles]
  (->> (colliding-tuples circles)
       (resolve-collisions circles)
       (map move)))

(def frame (.-requestAnimationFrame js/window))

(defn animate! [circles]
  (.clearRect ctx 0 0 innerWidth innerHeight)
  (doseq [c circles] (draw! c))
  (frame (partial animate! (step-world circles))))

(animate! (repeatedly 10 #(make-circle)))

; TODO
; deal with circles ending up inside others
; radius dynamic
; spawn so nothing overlaps
; gravity?
; do broad phase - grid
