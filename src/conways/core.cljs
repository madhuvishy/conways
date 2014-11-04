(ns conways.core
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]))

(enable-console-print!)

(def app-state (atom {:width 8 :height 8 :cells []}))

(def offsets [[-1 -1] [0 -1] [1 -1]
              [-1  0]        [1  0]
              [-1  1] [0  1] [1  1]])

(defn alive-neighbors [r c cells]
  (reduce + (filter 
              identity 
              (map (fn [[offsetx offsety]] 
                     (let [x (+ offsetx r) y (+ offsety c)]
                       (get-in cells [x y]))) offsets))))

(defn initial-grid [width height]
  (vec (for [r (range height)] 
         (vec (for [c (range width)] (rand-int 2))))))

(defn alive? [cell] (= cell 1))

(defn row [width cells r]
  (apply dom/div #js {:className "row"}
         (map
           (fn [c] 
             (let [cell (get-in cells [r c])]
               (dom/div #js {:className (if (alive? cell) "alive cell" "dead cell")})))
           (range width))))

(defn world [{:keys [width height cells] :as app} owner]
  (reify
    om/IDidMount 
    (did-mount [_]
      (om/transact! app :cells (fn [_] (initial-grid width height)))
      (prn (alive-neighbors 4 4 cells)))

    om/IRender
    (render [_]
      (dom/div #js {:ref "cells" :id "cells"}
               (apply dom/div nil
                      (map (fn [r] (row width cells r)) (range height)))))))

(om/root
  (fn [app owner]
    (reify 
      om/IRender
      (render [_]
        (dom/div nil 
                 (om/build world app)))))
  app-state
  {:target (. js/document (getElementById "app"))})
