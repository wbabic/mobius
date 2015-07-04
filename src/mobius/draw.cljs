(ns mobius.draw
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [om.core :as om :include-macros true]
            [cljs.core.async :as async :refer [>! <! put! chan alts! timeout]]
            [cljs.core.match :refer-macros [match]]
            [mobius.events :as e]
            [mobius.vector :as v]
            [mobius.geometry :as geom]
            [mobius.geometry.circle :as c]))

(enable-console-print!)

(def radius 2)

;; functions to render basic geometric primitives to a canvas context
(defn point [context [x y]]
  (.beginPath context)
  (.arc context x y radius 0 (* 2 Math/PI) false)
  (.stroke context)
  (.fill context)
  (.closePath context))

(defn line [context p1 p2]
  (let [[x1 y1] p1
        [x2 y2] p2]
    (.beginPath context)
    (.moveTo context x1 y1)
    (.lineTo context x2 y2)
    (.stroke context)
    (.closePath context)))

(defn triangle [context p1 p2 p3]
  (let [[x1 y1] p1
        [x2 y2] p2
        [x3 y3] p3]
    (.beginPath context)
    (.moveTo context x1 y1)
    (.lineTo context x2 y2)
    (.lineTo context x3 y3)
    (.fill context)
    (.closePath context)))

(defn rect [context [x1 y1] [x2 y2]]
  (.fillRect context x1 y1 x2 y2))

(defn circle [context center radius]
  (.beginPath context)
  (.arc context (center 0) (center 1) radius 0 (* 2 Math/PI) false)
  (.stroke context)
  ;;(.fill context)
  (.closePath context))

(defn arc
  "counter clockwise arc from start to end radians"
  [context center radius start end clockwise]
  (.beginPath context)
  (.arc context (center 0) (center 1) radius start end (not clockwise))
  (.stroke context)
  (.closePath context))

(defn disk [context center radius]
  (.beginPath context)
  (.arc context (center 0) (center 1) radius 0 (* 2 Math/PI) false)
  (.stroke context)
  (.fill context)
  (.closePath context))

(defn style [context s]
  (doseq [[k v] s]
    (case k
      :fill (set! (. context -fillStyle) v)
      :stroke (set! (. context -strokeStyle) v)
      :lineDash (set! (. context -setLineDash) v)
      :lineWidth (set! (. context -lineWidth) v))))

;; user space -> screen mapping stuff
(def round-pt (fn [p] (mapv Math.round p)))

(defn user->screen
  [config]
  (let [[xi xf] (:domain config)
        [yi yf] (:range config)
        [xres yres] (:resolution config)
        sx (/ xres (- xf xi))
        sy (/ yres (- yi yf))
        scale (v/scale sx sy)
        translate (v/translation [(- xi) (- yf)])]
    (fn [p]
      (if (number? p)
        (* sx p)
        ((comp round-pt scale translate) p)))))

(defn screen->user
  [config]
  (let [[xi xf] (:domain config)
        [yi yf] (:range config)
        [xres yres] (:resolution config)
        sx (/ xres (- xf xi))
        sy (/ yres (- yi yf))
        scale-inverse (v/scale (/ sx) (/ sy))
        translate-inverse (v/translation [xi yf])]
    (comp translate-inverse scale-inverse)))

(defn render
  [data context t-fn]
  (match data
         [:rect p1 p2]
         (rect context (t-fn p1) (t-fn p2))
         [:point p]
         (point context (t-fn p))
         [:line p1 p2]
         (line context (t-fn p1) (t-fn p2))
         [:triangle p1 p2 p3]
         (triangle context (t-fn p1) (t-fn p2) (t-fn p3))
         [:circle C]
         (let [{:keys [center radius]} C]
           (circle context (t-fn center) (t-fn radius)))
         [:arc C]
         (let [{:keys [center radius start end clockwise]} C]
           (arc context (t-fn center) (t-fn radius) (- start) (- end) clockwise))
         [:style s]
         (style context s)))

(defn drawing-loop
  ([id config]
   (let [draw-chan (chan)
         canvas (.getElementById js/document id)
         context (.getContext canvas "2d")
         t-fn (user->screen config)]
     (go (loop [t-fn t-fn]
           (let [draw-msg (<! draw-chan)]
             (render draw-msg context t-fn)
             (recur t-fn))))
     draw-chan))
  ([id config events?]
   (let [draw-chan (chan)
         canvas (.getElementById js/document id)
         context (.getContext canvas "2d")
         t-fn (user->screen config)
         f-inv (map (screen->user config))
         event-chan (e/mouse-events canvas f-inv)]
     (go (loop [t-fn t-fn]
           (let [draw-msg (<! draw-chan)]
             (render draw-msg context t-fn)
             (recur t-fn))))
     [draw-chan event-chan])))


(defn add-animation [id app-state]
  (om/transact! app-state :animations #(conj % id)))

(defn remove-animation [id app-state]
  (om/transact! app-state :animations #(disj % id)))

;; generic drawing utils
(def clear
  [[:style {:fill "grey"}]
   [:rect [-4 4] [4 -4]]])

(defn clear-screen [draw-chan]
  (go (doseq [d clear]
        (>! draw-chan d))))

(defn concentric-circles
  "generate sequence of circles
  with given center and radii in range"
  [center radii]
  (let [c (fn [r] [:circle {:center center :radius r}])]
    (for [r radii]
      (c r))))

(def colors ["red" "orange" "yellow" "green" "blue" "indigo" "violet"])

(defn draw-axis
  "send a sequence of real-axis imaginary-axis and unit circle
  with delay sending a message when done"
  [draw-chan-1 draw-chan-2 trans delay done]
  (let [axis geom/axis]
    (go
      (doseq [[c color] (map vector axis ["yellow" "blue" "cyan" "green" "magenta" "red"])]
        (<! (timeout delay))
        (>! draw-chan-1 [:style {:stroke color :lineWidth 1}])
        (>! draw-chan-1 c)
        (>! draw-chan-2 [:style {:stroke color :lineWidth 1}])
        (>! draw-chan-2 (trans c)))
      (<! (timeout 800))
      (>! done :axis))))

(defn render-axis
  "send a sequence of real-axis imaginary-axis and unit circle"
  [draw-chan-1 draw-chan-2 trans done]
  (let [axis geom/axis]
    (go
      (doseq [[c color]
              (map vector axis ["yellow" "blue" "cyan" "green" "magenta" "red"])]
        (>! draw-chan-1 [:style {:stroke color :lineWidth 1}])
        (>! draw-chan-1 c)
        (>! draw-chan-2 [:style {:stroke color :lineWidth 1}])
        (>! draw-chan-2 (trans c)))
      (>! done :axis))))

(defn draw-concentric-circles
  "send a sequence of circles to the drawing channel"
  [draw-chan-1 draw-chan-2 trans delay done]
  (let [circles (concentric-circles [0 0] (sort [1 1.5 (/ 2 3) 2 0.50 4 0.25]))]
    (go
      (doseq [[c color] (map vector circles colors)]
        (<! (timeout delay))
        (>! draw-chan-1 [:style {:stroke color :lineWidth 1}])
        (>! draw-chan-1 c)
        (>! draw-chan-2 [:style {:stroke color :lineWidth 1}])
        (>! draw-chan-2 (trans c)))
      (<! (timeout 800))
      (>! done :concentric-circles))))

(defn render-concentric-circles
  "send a sequence of circles to the drawing channel"
  [draw-chan-1 draw-chan-2 trans done]
  (let [circles (concentric-circles [0 0] (sort [1 1.5 (/ 2 3) 2 0.50 4 0.25]))]
    (go
      (doseq [[c color] (map vector circles colors)]
        (>! draw-chan-1 [:style {:stroke color :lineWidth 1}])
        (>! draw-chan-1 c)
        (>! draw-chan-2 [:style {:stroke color :lineWidth 1}])
        (>! draw-chan-2 (trans c)))
      (>! done :concentric-circles))))

(defn draw-radial-lines
  "send a sequence of radial lines to the drawing channel"
  [draw-chan-1 draw-chan-2 trans delay done]
  (let [lines (geom/radial-lines 12)]
    (go
      (doseq [[l c] (map vector lines (cycle colors))]
        (<! (timeout delay))
        (>! draw-chan-1 [:style {:stroke c :lineWidth 1}])
        (>! draw-chan-1 l)
        (>! draw-chan-2 [:style {:stroke c :lineWidth 1}])
        (>! draw-chan-2 (trans l)))
      (<! (timeout 800))
      (>! done :radial-lines))))

(defn render-radial-lines
  "send a sequence of radial lines to the drawing channel"
  [draw-chan-1 draw-chan-2 trans done]
  (let [lines (geom/radial-lines 12)]
    (go
      (doseq [[l c] (map vector lines (cycle colors))]
        (>! draw-chan-1 [:style {:stroke c :lineWidth 1}])
        (>! draw-chan-1 l)
        (>! draw-chan-2 [:style {:stroke c :lineWidth 1}])
        (>! draw-chan-2 (trans l)))
      (>! done :radius))))

(defn draw-horizontal-lines
  "send a sequence of horizontal lines to the drawing channel"
  [draw-chan-1 draw-chan-2 trans delay done]
  (let [lines (geom/horizontal-lines 0.50)]
    (go
      (doseq [[l c] (map vector lines (repeat "blue"))]
        (<! (timeout delay))
        (>! draw-chan-1 [:style {:stroke c :lineWidth 1}])
        (>! draw-chan-1 l)
        (>! draw-chan-2 [:style {:stroke c :lineWidth 1}])
        (>! draw-chan-2 (trans l)))
      (<! (timeout 800))
      (>! done :horizontal-lines))))

(defn render-horizontal-lines
  "send a sequence of horizontal lines to the drawing channel"
  [draw-chan-1 draw-chan-2 trans done]
  (let [lines (geom/horizontal-lines 0.50)]
    (go
      (doseq [[l c] (map vector lines (repeat "blue"))]
        (>! draw-chan-1 [:style {:stroke c :lineWidth 1}])
        (>! draw-chan-1 l)
        (>! draw-chan-2 [:style {:stroke c :lineWidth 1}])
        (>! draw-chan-2 (trans l)))
      (>! done :horizontal-lines))))

(defn draw-vertical-lines
  "send a sequence of vertical lines to the drawing channel"
  [draw-chan-1 draw-chan-2 trans delay done]
  (let [lines (geom/vertical-lines 0.50)]
    (go
      (doseq [[l c] (map vector lines (repeat "orange"))]
        (<! (timeout delay))
        (>! draw-chan-1 [:style {:stroke c :lineWidth 1}])
        (>! draw-chan-1 l)
        (>! draw-chan-2 [:style {:stroke c :lineWidth 1}])
        (>! draw-chan-2 (trans l)))
      (<! (timeout 800))
      (>! done :vertical-lines))))

(defn render-vertical-lines
  "send a sequence of vertical lines to the drawing channel"
  [draw-chan-1 draw-chan-2 trans done]
  (let [lines (geom/vertical-lines 0.50)]
    (go
      (doseq [[l c] (map vector lines (repeat "orange"))]
        (>! draw-chan-1 [:style {:stroke c :lineWidth 1}])
        (>! draw-chan-1 l)
        (>! draw-chan-2 [:style {:stroke c :lineWidth 1}])
        (>! draw-chan-2 (trans l)))
      (>! done :vertical-lines))))

;; animation functions
(defn animate
  "perform an animation of given data with given delay"
  [app-state animation-data-key delay draw-chan-1 draw-chan-2 trans]
  ;; set animation-mode to true
  (add-animation animation-data-key app-state)

  ;; send data to draw-channels
  (let [done (chan)]
    (condp = animation-data-key
      :axis
      (draw-axis draw-chan-1 draw-chan-2 trans 800 done)
      :concentric-circles
      (draw-concentric-circles draw-chan-1 draw-chan-2 trans 800 done)
      :radial-lines
      (draw-radial-lines draw-chan-1 draw-chan-2 trans 800 done)
      :horizontal-lines
      (draw-horizontal-lines draw-chan-1 draw-chan-2 trans 800 done)
      :vertical-lines
      (draw-vertical-lines draw-chan-1 draw-chan-2 trans 800 done)
      )
    ;; when done, set animation-mode to false
    (go (let [animation-data-key (<! done)]
          (remove-animation animation-data-key app-state)))))

(defn render-data
  "render data to drawing channels"
  [app-state draw-chan-1 draw-chan-2 trans done]
  (let [render-list (:render-list app-state)
        ret-chan (chan)
        n (count render-list)]
    (doseq [key render-list]
      (match key
             :axis
             (render-axis draw-chan-1 draw-chan-2 trans ret-chan)
             :concentric-circles
             (render-concentric-circles draw-chan-1 draw-chan-2 trans ret-chan)
             :radial-lines
             (render-radial-lines draw-chan-1 draw-chan-2 trans ret-chan)
             :horizontal-lines
             (render-horizontal-lines draw-chan-1 draw-chan-2 trans ret-chan)
             :vertical-lines
             (render-vertical-lines draw-chan-1 draw-chan-2 trans ret-chan)))
    (go
      (loop [c 0]
        (when (< c n)
          (<! ret-chan)
          (recur (inc c))))
      (>! done :done))))

(defn render-mouse-point
  [state draw-chan-1 draw-chan-2 trans]
  (let [rect? (get-in state [:mouse-mode :rectangular])
        polar? (get-in state [:mouse-mode :polar])
        point (:mouse-point state)
        f #(mapv trans %)]
    (when polar?
      (let [line (c/radial-line-through-point point)
            circle (c/circle-through-point point)
            render-list [[line c/cs-1] [circle c/cs-2]]]
        (go
          (doseq [[r cs] render-list]
            (doseq [d (c/render r cs)]
              (>! draw-chan-1 d))
            (doseq [d (c/render (f r) cs)]
              (>! draw-chan-2 d))))))
    (when rect?
      (let [h-line (c/horizontal-line-through-point point)
            v-line (c/vertical-line-through-point point)
            render-list [[h-line c/cs-1] [v-line c/cs-2]]]
        (go
          (doseq [[r cs] render-list]
            (doseq [d (c/render r cs)]
              (>! draw-chan-1 d))
            (doseq [d (c/render (f r) cs)]
              (>! draw-chan-2 d))))))))

(defn render-local
  [app-state local-state ch-1 ch-2 trans]
  (let [ret-chan (chan)]
    (go
      (clear-screen ch-1)
      (clear-screen ch-2)
      (render-data app-state ch-1 ch-2 trans ret-chan)
      ;; wait for render-data to complete
      (<! ret-chan)
      ;; now draw local-state on top
      (render-mouse-point local-state ch-1 ch-2 trans))))
