(ns conways.core
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]))

(enable-console-print!)

(def app-state (atom {:dimensions [30 30] :cells []}))

(def offsets [[-1 -1] [0 -1] [1 -1]
              [-1  0]        [1  0]
              [-1  1] [0  1] [1  1]])

(defn initial-grid 
  ([dimensions] (initial-grid dimensions true))
  ([[width height] random?]
   (vec (for [r (range height)] 
          (vec (for [c (range width)] (if random? (rand-int 2) 0)))))))

(defn valid? [[w h] [x y]]
  (and (>= x 0) (< x w) (>= y 0) (< y h)))

(defn alive-neighbors [r c {:keys [dimensions cells]}]
  (reduce + (filter 
              identity 
              (map (fn [[offsetx offsety]] 
                     (let [x (+ offsetx r) y (+ offsety c)]
                       (get-in cells [x y]))) offsets))))

(defn alive? [cell] (= cell 1))

(defn living? [r c {:keys [dimensions cells] :as state}]
  (let [neighbors (alive-neighbors r c state)
        alive (alive? (get-in cells [r c]))]
    (cond (and alive (or (< neighbors 2) (> neighbors 3))) false
          (and (not alive) (= neighbors 3)) true
          :else alive)))

(defn next-gen [{:keys [dimensions cells] :as state}]
  (let [[width height] dimensions
        next-cells (atom (initial-grid dimensions false))]
    (doseq [r (range height)
            c (range width)]
      (swap! next-cells assoc-in [r c] (if (living? r c state) 1 0)))
    @next-cells))

(defn row [width cells r]
  (apply dom/div #js {:className "row"}
         (map
           (fn [c] 
             (let [cell (get-in cells [r c])]
               (dom/div #js {:className (if (alive? cell) "alive cell" "dead cell")})))
           (range width))))

(defn world [{:keys [dimensions cells] :as state} owner]
  (reify
    om/IWillMount 
    (will-mount [_]
      (om/update! cells (initial-grid dimensions))
      (js/setInterval
        (fn [_] (om/update! cells (next-gen @state)))
        100))

    om/IRender
    (render [_]
      (let [[width height] dimensions]
        (dom/div #js {:ref "cells" :id "cells"}
                 (apply dom/div nil
                        (map (fn [r] (row width cells r)) (range height))))))))

(om/root
  (fn [app owner]
    (reify 
      om/IRender
      (render [_]
        (dom/div nil 
                 (om/build world app)))))
  app-state
  {:target (. js/document (getElementById "app"))})
