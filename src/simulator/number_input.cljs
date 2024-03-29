(ns simulator.number-input
  (:require [reagent.core :as r]))

(defn add-event-listener! [e f] (.addEventListener js/document e f))
(defn remove-event-listener! [e f] (.removeEventListener js/document e f))

(defn clamp [opts n] (-> n (max (:min opts)) (min (:max opts))))

(defn draggable-input
  [{:keys [label value swap-value increment text-width width]
    :or   {increment 1
           width 40}}
   init]
  (let [drag-state (atom 0)
        hover? (r/atom false)]
    (fn [{:keys [label value increment swap-value text-width width disabled?]
          :or   {increment 1
                 text-width 40
                 width 40}
          :as opts}
         init]
      (letfn [(on-mouse-down [e]
                (reset! drag-state (.-clientX e))
                (add-event-listener! "mouseup" on-mouse-up)
                (add-event-listener! "mousemove" on-mouse-move))
              (on-mouse-up [e]
                (remove-event-listener! "mouseup" on-mouse-up)
                (remove-event-listener! "mousemove" on-mouse-move))
              (on-mouse-move [e]
                (let [x0 @drag-state
                      x1 (.-clientX e)
                      delta (- x1 x0)
                      absdelta (.abs js/Math delta)
                      abs-value-change (* increment (/ absdelta 2))
                      value-change (if (pos? delta)
                                     (.floor js/Math abs-value-change)
                                     (- (.ceil js/Math abs-value-change)))]
                  (when-not (zero? value-change)
                    (reset! drag-state x1))
                  (swap-value
                    (fn [n]
                      (clamp opts
                        (if-not n
                          (or init 0)
                          (+ n value-change)))))))]
        [:div
         {:style {:display       :flex
                  :border-radius 4
                  :width         (+ width text-width)
                  :font-size     "0.85em"
                  :margin-right  5}
          :onMouseOver (fn [e] (reset! hover? true))
          :onMouseOut (fn [e] (reset! hover? false))}
         [:span {:onMouseDown (when-not disabled? on-mouse-down)
                 :style {:padding 5
                         :padding-top 2
                         :padding-bottom 2
                         :padding-left 2
                         :color :gray
                         :cursor (when-not disabled? :ew-resize)
                         :userSelect :none
                         :width text-width}}
          label]
         [:input
          {:type "number"
           :value value
           :disabled disabled?
           :onChange (fn [e]
                       (swap-value
                         (fn [_]
                           (clamp opts
                                  (js/parseFloat (.-value (.-target e)))))))
           :style    {:width width
                      :border :none
                      :outline :none
                      :font-family :serif
                      :font-size "1em"}}]]))))
