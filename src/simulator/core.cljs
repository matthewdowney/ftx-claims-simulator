(ns simulator.core
  (:require [reagent.dom :refer [render]]
            [reagent.core :as r]
            ["plotly.js-dist-min" :as plotly]
            ["seedrandom" :as seedrandom]
            [simulator.number-input :as ni]
            [kixi.stats.core :as stats]))


(def initial-model-controls
  {:claim-price 10

   :win-value 30
   :lose-value 3
   :ruin-value 0

   :win-prob 45
   :lose-prob 45
   :ruin-prob 10})

(defonce state
  (r/atom
    (assoc initial-model-controls
     :bet-size 10
     :portfolios 100
     :bets 100)))

(defn bet-on-claim
  [{:keys [lose-prob ruin-prob win-value lose-value ruin-value]} rng]
  (let [rnd (* (rng) 100)]
    (cond
      (< rnd ruin-prob) ruin-value
      (< rnd (+ ruin-prob lose-prob)) lose-value
      :else win-value)))

(defn bets* [portfolio-value {:keys [claim-price bet-size] :as params} rng]
  (lazy-seq
    (let [; Pick a bet size as some fraction of the portfolio > 0
          bet-amount (max (* portfolio-value (/ bet-size 100.0)) 0)
          n-claims (/ bet-amount claim-price)
          ; Compute the result from the bet and its effect on the portfolio
          bet-value (* (bet-on-claim params rng) n-claims)
          new-portfolio-value (-> portfolio-value (- bet-amount) (+ bet-value))]
      (cons portfolio-value (bets* new-portfolio-value params rng)))))

(defn plot [data]
  (let [plot-id (str (gensym))]
    (r/create-class
      {:display-name "custom-plot-component"
       :reagent-render (fn [_] [:div {:id plot-id :class "plotly-plot"}])
       :component-did-mount (fn [this] (.newPlot plotly plot-id (clj->js data)))
       :component-did-update
       (fn [this old-argv]
         (let [new-argv (rest (r/argv this))]
           (.react plotly plot-id (clj->js (first new-argv)))))})))

(defn abbrev
  "Abbreviate a number, e.g. 11153.23 => 11.1k."
  [n min-sigs]
  (let [round (fn [n]
                (let [scale (Math/floor (Math/log10 n))
                      round-to (if (neg? scale)
                                 (- min-sigs scale)
                                 (max (- min-sigs (inc scale)) 0))]
                  (.toFixed n round-to)))]
    (cond
      (> n 1000000000000) (str (round (/ n 1000000000000.0)) "t")
      (> n 1000000000) (str (round (/ n 1000000000.0)) "b")
      (> n 1000000) (str (round (/ n 1000000.0)) "m")
      (> n 1000) (str (round (/ n 1000.0)) "k")
      (zero? n) "0"
      :else (str (round n)))))

(defn -simulate [params]
  (let [rng (seedrandom 1)
        n-portfolios (:portfolios params)
        n-bets (:bets params)]
    (mapv ; simulate 100 portfolios
      (fn [_]
        {:y          (into [] (take n-bets) (bets* 1.0 params rng))
         :opacity    0.15
         :showlegend false
         :type       :scatter})
      (range n-portfolios))))

(defn -medians [simulated params]
  (mapv
    (fn [at-time]
      (let [portfolios (vec
                         (sort
                           (map
                             #(nth (:y %) at-time)
                             simulated)))]
        (nth portfolios (quot (count portfolios) 2))))
    (range (:bets params))))

(defn -stats [simulated medians]
  (let [final-vals (mapv (comp peek :y) simulated)
        stdev (transduce identity stats/standard-deviation final-vals)
        mean (transduce identity stats/mean final-vals)
        med (peek medians)]
    {:mean mean
     :median med
     :stdev stdev
     :n (count final-vals)
     :n-gained (count (filter #(> % 1) final-vals))
     :n-lost (count (filter #(< % 1) final-vals))}))

(defn clamp [min' x max'] (-> x (min max') (max min')))
;; TODO: Compute medians separately (incrementally) for a speed boost?
(defn plot-simulation [simulated medians {:keys [mean median stdev]}]
  (let [m {:y          medians
           :width      3
           :showlegend true
           :type       :scatter
           :name       "Median portfolio"}]
    [plot
     {:data   (clj->js (conj simulated m))
      :layout {:title (str "Median return multiple: "
                           (abbrev median 4)
                           " ± " (abbrev stdev 4)
                           (str " (μ = " (abbrev mean 4) ")"))
               :xaxis {:title "Bet #"}
               :yaxis {:title "Bankroll" :type :log}
               :width (- (clamp 550 (.-innerWidth js/window) 950) 50)}}]))

(defn adjust-probabilities
  "Adjust probabilities so that they add up to 100."
  [{:keys [win-prob lose-prob ruin-prob] :as state} changed-key]
  (if (and (contains? #{:win-prob :lose-prob :ruin-prob} changed-key))
    (let [adjust (- 100 (+ win-prob lose-prob ruin-prob))
          [to-adjust nxt] (case changed-key
                            :win-prob [:lose-prob :lose-prob]
                            :lose-prob [:win-prob :ruin-prob]
                            :ruin-prob [:win-prob :win-prob])]
      (if-not (zero? adjust)
        (recur
          (update state to-adjust (fn [x] (-> x (+ adjust) (max 0) (min 100))))
          nxt)
        state))
    state))

(defn model-parameter [s label k props]
  (let [v (get s k)]
    [ni/draggable-input
     (merge
       {:label label
        :value v
        :swap-value (fn [f]
                      (swap! state
                        (fn [s]
                          (-> s
                              (update k f)
                              (adjust-probabilities k)))))
        :min 0
        :max 100
        :text-width 125
        :width 50}
       props)
     v]))

(defn model-controls [s]
  [:div {:style {:line-height 1.1}}
   [:p {:style {:padding 0 :margin 0 :margin-bottom "0.25em"}} "Model inputs"]
   [model-parameter s "claim price" :claim-price {}]
   [model-parameter s "win value" :win-value {:max 500}]
   [model-parameter s "lose value" :lose-value {}]
   [model-parameter s "ruin value" :ruin-value {}]
   [model-parameter s "win probability" :win-prob {}]
   [model-parameter s "lose probability" :lose-prob {}]
   [model-parameter s "ruin probability" :ruin-prob {}]
   [:button {:style {:font-size "0.60em"
                     :color :gray
                     :background :none
                     :border :none
                     :cursor :pointer}
             :onClick (fn [_] (swap! state merge initial-model-controls))}
    "Reset"]])

(defn simulation-controls [s]
  [:div {:style {:line-height 1.1}}
   [:p {:style {:padding 0 :margin 0 :margin-bottom "0.25em"}} "Simulation controls"]
   [model-parameter s "bet size" :bet-size {}]
   [model-parameter s "portfolios" :portfolios {:min 1 :max 1000}]
   [model-parameter s "bets" :bets {:min 10 :max 1000}]])

(defn app []
  (let [{:keys [claim-price win-prob lose-prob ruin-prob win-value lose-value ruin-value portfolios bets bet-size]
         :as s} @state]
    (let [simulated (-simulate s)
          medians (-medians simulated s)
          stats (-stats simulated medians)

          ev (/ (+ (* win-prob win-value)
                   (* lose-prob lose-value)
                   (* ruin-prob ruin-value))
                100.0)]
      [:div
       [:div {:style {:display :flex
                      :flex-wrap :wrap
                      :align-items :flex-start
                      :justify-content :center}}
        [model-controls s]
        [plot-simulation simulated medians stats]
        [simulation-controls s]]
       [:div {:style {:display :flex :align-items :center :justify-content :space-around}}
        [:div {:style {:display :flex
                       :align-items :center
                       :justify-content :space-around
                       :max-width 800}}
         [:table {:style {:color :gray
                          :font-size "0.75em"}}
          [:tbody
           [:tr [:td "Mean"] [:td (:mean stats)]]
           [:tr [:td "Median"] [:td (:median stats)]]
           [:tr [:td "Stdev"] [:td (:stdev stats)]]
           [:tr [:td "N"] [:td (:n stats)]]
           [:tr [:td "N(Gained)"] [:td (:n-gained stats)]]
           [:tr [:td "N(Lost)"] [:td (:n-lost stats)]]]]

         [:div {:style {:margin-left "1em"}}
          [:p "Simulation of " portfolios " portfolios betting "
           [:strong bet-size "%"] " of their bankroll on each of "
           bets " bets."]
          [:p "Each claim costs "  [:strong claim-price "¢"] " and has a "
           [:strong win-prob "%"] " chance of resolving to "
           [:strong win-value "¢, "]
           "a " [:strong lose-prob "%"] " chance of resolving to "
           [:strong lose-value "¢, "]
           "and a " [:strong ruin-prob "%"] " chance of resolving to "
           [:strong ruin-value "¢"] ", for an EV of " [:strong ev "¢"]]]]]])))


(defn stop []
  (js/console.log "Stopping..."))

(defn start []
  (js/console.log "Starting...")
  (render [app] (.getElementById js/document "app")))

(defn ^:export init []
  (start))
