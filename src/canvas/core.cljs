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

; (defn resolve-collision
;   [circles]
;   (map #(update :dx -) circles))

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

(defn animate! [circles]
  (.clearRect ctx 0 0 innerWidth innerHeight)
  (doseq [c circles] (draw! c))
  (when-let [a (not-empty (colliding-tuples circles))] (println a))
  (.requestAnimationFrame js/window (partial animate! (map move-with-wall-detection circles))))

(animate! (repeatedly 2 #(make-circle)))

;(defn on-js-reload [])
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
