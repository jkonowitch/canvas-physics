(ns canvas.core
    (:require))

(enable-console-print!)

(def canvas (.querySelector js/document "canvas"))
(def ctx (.getContext canvas "2d"))

(def innerWidth (.-innerWidth js/window))
(def innerHeight (.-innerHeight js/window))

(set! (.-height canvas) innerHeight)
(set! (.-width canvas) innerWidth)

(def two-radians (* 2 (.-PI js/Math)))
(def diameter 60)
(def radius (/ diameter 2))

(defn rand-betw [a b]
  (+ a (rand (- b a))))

(defn make-circle []
  {:x (rand-betw radius (- innerWidth radius))
   :y (rand-betw radius (- innerHeight radius))
   :dx (* (rand) 5)
   :dy (* (rand) 5)})

(defn in-bounds? [outer-bound]
  (fn [point] (< radius point (- outer-bound radius))))

(def in-bounds-x? (in-bounds? innerWidth))
(def in-bounds-y? (in-bounds? innerHeight))

(defn resolve-wall-y
  [{y :y :as circle}]
  (if-not (in-bounds-y? y)
    (-> circle
        (update :dy -)
        (as-> c (update c :y + (:dy c))))
    circle))

(defn resolve-wall-x
  [{x :x :as circle}]
  (if-not (in-bounds-x? x)
    (-> circle
        (update :dx -)
        (as-> c (update c :x + (:dx c))))
    circle))

(defn move
  [{dx :dx dy :dy :as circle}]
  (-> circle
      (update :x + dx)
      (update :y + dy)))

(def move-with-wall-detection (comp resolve-wall-x resolve-wall-y  move))

(defn draw! [circle]
   (.beginPath ctx)
   (set! (.-strokeStyle ctx) "blue")
   (set! (.-fillStyle ctx) "purple")
   (.arc ctx (:x circle) (:y circle) radius 0 two-radians)
   (.stroke ctx)
   (.fill ctx))

(defn animate! [circles]
  (.clearRect ctx 0 0 innerWidth innerHeight)
  (doseq [c circles] (draw! c))
  (.requestAnimationFrame js/window (partial animate! (map move-with-wall-detection circles))))

(animate! (repeatedly 5 #(make-circle)))

;(defn on-js-reload [])
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
