(ns mobius.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [cljs.core.async :as async :refer [chan <! >! timeout]]
            [mobius.draw :as draw]
            [mobius.geometry :as geom]
            [mobius.transforms :as t]))

(enable-console-print!)

(println "mobius.core")

(def canvas-1-config
  {:resolution [500 500]
   :domain [-4 4]
   :range [-4 4]})

(def canvas-2-config
  {:resolution [500 500]
   :domain [-4 4]
   :range [-4 4]})

(defonce app-state
  (atom
   {:index 0
    :transforms t/transforms
    :render-list []}))

(defn transform
  "return current transform from dereferences state"
  [state]
  (let [i (:index state)]
    (get (:transforms state) i)))

(defn t-fn
  "return current transform from dereferenced state"
  [state]
  (let [t (transform state)]
    (:transform t)))

(defn el [id] (js/document.getElementById id))

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
  (println "reloading mobius.core ...")
  )

(defn update-local-state [e owner key state]
  (om/set-state! owner key (.. e -target -value)))

(defn input [key value owner state]
  (dom/input #js {:type "text"
                  :value value
                  :onChange
                  #(update-local-state % owner key state)}))

(defn update-state [e app-state index]
  (println "update-state" (.. e -target -value) ":" index)
  (om/update! app-state [:index] index))

(def toggle
  (fn [value] (if (true? value) false true)))

(defn update-mouse-mode [e owner]
  (let [target (. e -target)
        name (. target -name)
        key (keyword (. target -value))]
    (om/update-state! owner [:mouse-mode key] toggle)
    (println "update: " name " value: " key)))

(defn mouse-mode [owner]
  (dom/dl nil
          (dom/h3 nil "Mouse Mode")
          (dom/dt nil "Polar")
          (dom/dd nil
                  (dom/input #js {:type "checkbox"
                                  :name "mouse-mode"
                                  :value "polar"
                                  :onChange
                                  #(update-mouse-mode % owner)}))
          (dom/dt nil "Rect")
          (dom/dd nil
                  (dom/input #js {:type "checkbox"
                                  :name "mouse-mode"
                                  :value "rectangular"
                                  :onChange
                                  #(update-mouse-mode % owner)}))))

(defn select [index value checked app-state]
  (if checked
    (dom/input #js {:type "radio"
                    :value value
                    :checked "checked"})
    (dom/input #js {:type "radio"
                    :value value
                    :onChange
                    #(update-state % app-state index)})))

(defn transform-item [t index current-index app-state]
  (let [name (:name t)
        text (:text t)]
    (list
     (dom/dt nil name)
     (dom/dd #js {:className "transform"}
             text
             (select index name (= index current-index) app-state)))))

(defn transform-items
  "List available transforms and a means to select current transform"
  [app-state]
  (let [transforms (:transforms @app-state)
        current-index (:index @app-state)]
    (apply dom/dl nil
           (flatten (for [[t i] (mapv vector transforms (range))]
                      (transform-item t i current-index app-state))))))

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
  "send a sequence of real-axis imaginary-axis and unit circle"
  [draw-chan-1 draw-chan-2]
  (let [axis geom/axis
        trans #(t/image (t-fn @app-state) %)]
    (go
      (doseq [[c color] (map vector axis ["yellow" "blue" "cyan" "green" "magenta" "red"])]
        (<! (timeout 800))
        (>! draw-chan-1 [:style {:stroke color :lineWidth 1}])
        (>! draw-chan-1 c)
        (>! draw-chan-2 [:style {:stroke color :lineWidth 1}])
        (>! draw-chan-2 (trans c))))))

(defn draw-concentric-circles
  "send a sequence of circles to the drawing channel"
  [draw-chan-1 draw-chan-2]
  (let [circles (concentric-circles [0 0] (sort [1 1.5 (/ 2 3) 2 0.50 4 0.25]))
        trans #(t/image (t-fn @app-state) %)]
    (go
      (doseq [[c color] (map vector circles colors)]
        (<! (timeout 800))
        (>! draw-chan-1 [:style {:stroke color :lineWidth 1}])
        (>! draw-chan-1 c)
        (>! draw-chan-2 [:style {:stroke color :lineWidth 1}])
        (>! draw-chan-2 (trans c))))))

(defn draw-radial-lines
  "send a sequence of radial lines to the drawing channel"
  [draw-chan-1 draw-chan-2]
  (let [lines (geom/radial-lines 12)
        trans #(t/image (t-fn @app-state) %)]
    (go
      (doseq [[l c] (map vector lines (cycle colors))]
        (<! (timeout 800))
        (>! draw-chan-1 [:style {:stroke c :lineWidth 1}])
        (>! draw-chan-1 l)
        (>! draw-chan-2 [:style {:stroke c :lineWidth 1}])
        (>! draw-chan-2 (trans l))))))

(defn draw-horizontal-lines
  "send a sequence of horizontal lines to the drawing channel"
  [draw-chan-1 draw-chan-2]
  (let [lines (geom/horizontal-lines 0.50)
        trans #(t/image (t-fn @app-state) %)]
    (go
      (doseq [[l c] (map vector lines (repeat "blue"))]
        (<! (timeout 800))
        (>! draw-chan-1 [:style {:stroke c :lineWidth 1}])
        (>! draw-chan-1 l)
        (>! draw-chan-2 [:style {:stroke c :lineWidth 1}])
        (>! draw-chan-2 (trans l))))))

(defn draw-vertical-lines
  "send a sequence of vertical lines to the drawing channel"
  [draw-chan-1 draw-chan-2]
  (let [lines (geom/vertical-lines 0.50)
        trans #(t/image (t-fn @app-state) %)]
    (go
      (doseq [[l c] (map vector lines (repeat "orange"))]
        (<! (timeout 800))
        (>! draw-chan-1 [:style {:stroke c :lineWidth 1}])
        (>! draw-chan-1 l)
        (>! draw-chan-2 [:style {:stroke c :lineWidth 1}])
        (>! draw-chan-2 (trans l))))))

(defn mobius-config
  "input form for mobius"
  [app-state owner]
  (reify
    om/IInitState
    (init-state [_]
      {:mouse-mode {:polar false
                    :rectangular false}})
    om/IWillMount
    (will-mount [_]
      (let [event-chan (om/get-shared owner :event-chan-1)
            control-chan (om/get-shared owner :control-chan)
            return-chan (chan)]
        (go (loop []
              (let [control-type (<! control-chan)]
                (condp = control-type
                  :mouse-mode
                  (do
                    )
                  ))))))

    om/IRenderState
    (render-state [_ state]
      (let [mouse-mode-state (:mouse-mode state)
            _ (prn mouse-mode-state)
            draw-chan-1 (om/get-shared owner :draw-chan-1)
            draw-chan-2 (om/get-shared owner :draw-chan-2)]
        (dom/div nil
                 (transform-items app-state)

                 (dom/button #js {:onClick
                                  #(do
                                     (draw-axis draw-chan-1
                                                draw-chan-2))}
                             "Axes and Unit Circle")
                 (dom/button #js {:onClick
                                  #(do
                                     (draw-concentric-circles draw-chan-1
                                                              draw-chan-2))}
                             "Concentric Circles")
                 (dom/button #js {:onClick
                                  #(do
                                     (draw-radial-lines draw-chan-1
                                                        draw-chan-2))}
                             "Radial Lines")
                 (dom/button #js {:onClick
                                  #(do
                                     (draw-horizontal-lines draw-chan-1
                                                            draw-chan-2))}
                             "Horizontal Lines")
                 (dom/button #js {:onClick
                                  #(do
                                     (draw-vertical-lines draw-chan-1
                                                          draw-chan-2))}
                             "Vertical Lines")
                 (dom/button #js {:onClick
                                  #(do
                                     (clear-screen draw-chan-1)
                                     (clear-screen draw-chan-2))}
                             "Clear")
                 (dom/div #js {:className "mouse-mode"}
                          (mouse-mode owner)))))))

(om/root
 mobius-config
 app-state
 {:target (el "mobius-config")
  :shared (let [[draw-chan-1 event-chan-1]
                (draw/drawing-loop "mobius-canvas-1"
                                   canvas-1-config
                                   [:mouse-move :mouse-down])
                draw-chan-2 (draw/drawing-loop "mobius-canvas-2" canvas-2-config)]
            {:draw-chan-1 draw-chan-1
             :event-chan-1 event-chan-1
             :draw-chan-2 draw-chan-2
             :control-chan (chan)})})

(comment
  (in-ns 'mobius.core)
  ;; test out user-screen mapping
  (let [m (draw/user->screen canvas-1-config)] [(m [0 0]) (m [1 0]) (m [0 1]) (m 1)])
  ;;=> [[250 250] [275 250] [250 225] 25]

  (let [m (draw/user->screen canvas-1-config)
        tf (map m)
        data [[0 0] [1 0] [0 1] 1 [-10 10] [10 -10]]]
    (sequence tf data))
  ;;=> ([250 250] [275 250] [250 225] 25 [0 0] [500 500])

  ;; test out t-fn
  (let [t-fn (draw/transform-fn canvas-1-config)
        data [[0 0] [1 0] [0 1] 1]]
    (sequence t-fn data))
  ;;=> ([250 250] [275 250] [250 225] 25)

  )
