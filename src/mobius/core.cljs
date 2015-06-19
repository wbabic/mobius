(ns mobius.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [cljs.core.async :as async :refer [chan <! >! timeout]]
            [mobius.draw :as draw]
            [mobius.geometry :as geom]
            [mobius.transforms :as t]))

(enable-console-print!)

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

(defn current-transform
  "return current transform from dereferences state"
  [state]
  (let [i (:index state)]
    (get (:transforms state) i)))

(defn transform-fn
  "return current transform from dereferenced state"
  [state]
  (let [t (current-transform state)]
    (:transform t)))

(defn image-fn [app-state]
  #(t/image (transform-fn @app-state) %))

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
  (println "reloading mobius.core ...")
  )

(defn update-transform-index [e app-state index]
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
  (dom/div #js {:className "mouse-mode"}
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
                                           #(update-mouse-mode % owner)})))))

(defn select-transform [index value checked app-state]
  (if checked
    (dom/input #js {:type "radio"
                    :value value
                    :checked "checked"})
    (dom/input #js {:type "radio"
                    :value value
                    :onChange
                    #(update-transform-index % app-state index)})))

(defn transform-item [t index current-index app-state]
  (let [name (:name t)
        text (:text t)]
    (list
     (dom/dt nil name)
     (dom/dd #js {:className "transform"}
             text
             (select-transform index name (= index current-index) app-state)))))

(defn transform-items
  "List available transforms and a means to select current transform"
  [app-state]
  (let [transforms (:transforms @app-state)
        current-index (:index @app-state)]
    (apply dom/dl nil
           (flatten (for [[t i] (mapv vector transforms (range))]
                      (transform-item t i current-index app-state))))))

(defn drawing-buttons
  "drawing buttons"
  [app-state draw-chan-1 draw-chan-2]
  (dom/div nil
           (dom/button #js {:onClick
                            #(do
                               (draw/draw-axis draw-chan-1
                                               draw-chan-2
                                               (image-fn app-state)
                                               800))}
                       "Axes and Unit Circle")
           (dom/button #js {:onClick
                            #(do
                               (draw/draw-concentric-circles
                                draw-chan-1
                                draw-chan-2
                                (image-fn app-state)
                                800))}
                       "Concentric Circles")
           (dom/button #js {:onClick
                            #(do
                               (draw/draw-radial-lines
                                draw-chan-1
                                draw-chan-2
                                (image-fn app-state)
                                800))}
                       "Radial Lines")
           (dom/button #js {:onClick
                            #(do
                               (draw/draw-horizontal-lines
                                draw-chan-1
                                draw-chan-2
                                (image-fn app-state)
                                800))}
                       "Horizontal Lines")
           (dom/button #js {:onClick
                            #(do
                               (draw/draw-vertical-lines
                                draw-chan-1
                                draw-chan-2
                                (image-fn app-state)
                                800))}
                       "Vertical Lines")
           (dom/button #js {:onClick
                            #(do
                               (draw/clear-screen draw-chan-1)
                               (draw/clear-screen draw-chan-2))}
                       "Clear")))

(defn handle-event
  "process move and click events from event-chan
  update local state till complete
  then return new triangle in ret-chan"
  [owner event-chan ret-chan]
  (go (loop [state {:step 0 :complete false}]
        (let [event (<! event-chan)
              new-state nil ;; (triangle-transitioner event state)
              _ (prn event)]
          (if (:complete new-state)
            (do
              ;; is complete, so do not recur
              ;; (om/set-state! owner new-state)
              (>! ret-chan :done))
            (do
              ;; not complete, keep going
              ;; (om/set-state! owner new-state)
              (recur new-state)))))))

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
              (let [control-type (<! control-chan)
                    _ (prn control-type)]
                (condp = control-type
                  :mouse-mode
                  (do
                    (handle-event owner event-chan return-chan))
                  ))))
        (go (>! control-chan :mouse-mode))))

    om/IRenderState
    (render-state [_ state]
      (let [mouse-mode-state (:mouse-mode state)
            _ (prn mouse-mode-state)
            draw-chan-1 (om/get-shared owner :draw-chan-1)
            draw-chan-2 (om/get-shared owner :draw-chan-2)]
        (dom/div nil
                 (transform-items app-state)
                 (drawing-buttons app-state draw-chan-1 draw-chan-2)
                 (mouse-mode owner))))))

(defn el [id] (js/document.getElementById id))

(om/root
 mobius-config
 app-state
 {:target (el "mobius-config")
  :shared (let [[draw-chan-1 event-chan-1]
                (draw/drawing-loop "mobius-canvas-1"
                                   canvas-1-config
                                   [:mouse-click])
                draw-chan-2 (draw/drawing-loop "mobius-canvas-2" canvas-2-config)]
            {:draw-chan-1 draw-chan-1
             :event-chan-1 event-chan-1
             :draw-chan-2 draw-chan-2
             :control-chan (chan)})})

(comment
  (in-ns 'mobius.core)
  ;; user-screen mapping
  (let [m (draw/user->screen canvas-1-config)
        tf (map m)
        data [[0 0] [1 0] [0 1] 1 [-10 10] [10 -10]]]
    (sequence tf data))
  ;;=> ([250 250] [313 250] [250 188] 62.5 [-375 -375] [875 875])

  ;; screen->user mapping
  (let [m-inv (draw/screen->user canvas-1-config)
        t-fn (map m-inv)
        data [[250 250] [313 250] [250 188] [-375 -375] [875 875]]]
    (sequence t-fn data))
  ;;=> ([0 0] [1.008 0] [0 0.992] [-10 10] [10 -10])
  )
