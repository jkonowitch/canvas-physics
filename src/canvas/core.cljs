(ns canvas.core
    (:require [clojure.math.combinatorics :as combo]))

(enable-console-print!)

(def canvas (.querySelector js/document "canvas"))
(def ctx (.getContext canvas "2d"))

(def innerWidth (.-innerWidth js/window))
(def innerHeight (.-innerHeight js/window))

(set! (.-height canvas) innerHeight)
(set! (.-width canvas) innerWidth)

(def diameter 60)
(def radius (/ diameter 2))

(def x-bound (- innerWidth radius))
(def y-bound (- innerHeight radius))

(defn rand-betw [a b]
  (+ a (rand (- b a))))

(defn make-circle []
  {:x (rand-betw radius x-bound)
   :y (rand-betw radius y-bound)
   :dx (rand-betw 1 5)
   :dy (rand-betw 1 5)
   :radius radius})

(defn resolve-wall-y
  [{y :y :as circle}]
  (if-not (< radius y y-bound)
    (-> circle
        (update :dy -)
        (as-> c (update c :y + (:dy c))))
    circle))

(defn resolve-wall-x
  [{x :x :as circle}]
  (if-not (< radius x x-bound)
    (-> circle
        (update :dx -)
        (as-> c (update c :x + (:dx c))))
    circle))

(defn move
  [{dx :dx dy :dy :as circle}]
  (-> circle
      (update :x + dx)
      (update :y + dy)))

(defn reverse-direction [c]
  (-> c
    (update :dx -)
    (update :dy -)))

(def move-with-wall-detection (comp resolve-wall-x resolve-wall-y  move))

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

(defn m-assoc [cs [i circle]]
  (assoc cs i circle))

(defn magnitude [v->]
  (->> v->
      (map #(.pow js/Math % 2))
      (reduce +)
      (.sqrt js/Math)))

(defn dot-product [& vs->]
  (reduce + (apply map * vs->)))

(defn one-d-velocity [v1 m1 v2 m2]
  (let [numerator (+ (* v1 (- m1 m2)) (* 2 m2 v2))
        denominator (+ m1 m2)]
    (/ numerator denominator)))

(defn collide-physics [[[i c1] [j c2] :as tupe]]
  (let [c1-> [(:x c1) (:y c1)]
        c2-> [(:x c2) (:y c2)]
        nrml-> (map - c1-> c2->)
        un-> (map #(/ % (magnitude nrml->)) nrml->)
        ut-> [(- (last un->)) (first un->)]
        vc1-> [(:dx c1) (:dy c1)]
        vc2-> [(:dx c2) (:dy c2)]
        vc1n (dot-product un-> vc1->)
        vc2n (dot-product un-> vc2->)
        vc1t (dot-product ut-> vc1->)
        vc2t (dot-product ut-> vc2->)
        vc1n' (one-d-velocity vc1n (:radius c1) vc2n (:radius c2))
        vc2n' (one-d-velocity vc2n (:radius c2) vc1n (:radius c1))
        vc1n->' (map (partial * vc1n') un->)
        vc2n->' (map (partial * vc2n') un->)
        vc1t->' (map (partial * vc1t) ut->)
        vc2t->' (map (partial * vc2t) ut->)
        vc1->' (map + vc1n->' vc1t->')
        vc2->' (map + vc2n->' vc2t->')
        c1' (merge c1 (zipmap [:dx :dy] vc1->'))
        c2' (merge c2 (zipmap [:dx :dy] vc2->'))]
    [[i c1'][j c2']]))

(def frame (.-requestAnimationFrame js/window))

(defn animate! [circles]
  (.clearRect ctx 0 0 innerWidth innerHeight)
  (doseq [c circles] (draw! c))
  (if-let [a (not-empty (colliding-tuples circles))]
    (do
      (let [b (apply concat (map collide-physics a))]
        (frame (partial animate! (map move-with-wall-detection (reduce m-assoc (vec circles) b))))))
    (frame (partial animate! (map move-with-wall-detection circles)))))

(animate! (repeatedly 5 #(make-circle)))

;(defn on-js-reload [])
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
