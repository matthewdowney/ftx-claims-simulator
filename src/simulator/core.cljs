(ns simulator.core
  (:require [reagent.dom :refer [render]]
            [reagent.core :as r]
            ["plotly.js-dist-min" :as plotly]
            [simulator.number-input :as ni]
            [rand-cljc.core :as rng]
            [kixi.stats.core :as stats]))

(defonce state
  (r/atom
    {:claim-price 10

     :win-prob 45
     :lose-prob 45
     :ruin-prob 5

     :win-value 30
     :lose-value 3
     :ruin-value 0

     :bet-size 10

     :portfolios 100
     :bets 100}))

(defn bet-on-claim
  [{:keys [lose-prob ruin-prob win-value lose-value ruin-value]} rng]
  (let [rnd (* (rng/rand rng) 100)]
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
       :reagent-render (fn [_] [:div {:id plot-id}])
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
  (let [rng (rng/rng 1)
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
  (let [stdev (transduce identity stats/standard-deviation medians)
        final-vals (mapv (comp peek :y) simulated)
        mean (transduce identity stats/mean final-vals)
        med (peek medians)]
    {:mean mean
     :median med
     :stdev stdev
     :n (count final-vals)
     :n-gained (count (filter #(> % 1) final-vals))
     :n-lost (count (filter #(< % 1) final-vals))}))

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
               :yaxis {:title "Bankroll" :type :log}}}]))

(defn simulation [params]
  (let [simulated (-simulate params)
        medians (-medians simulated params)
        stats (-stats simulated medians)]
    [:div {:style {:display :flex :flex-wrap :wrap :align-items :center :justify-content :space-around}}
     [plot-simulation simulated medians stats]
     [:table {:style {:color :gray :font-size "0.75em" :margin-left "-200px" :z-index 10}}
      [:tbody
      [:tr [:td "Mean"] [:td (:mean stats)]]
      [:tr [:td "Median"] [:td (:median stats)]]
      [:tr [:td "Stdev"] [:td (:stdev stats)]]
      [:tr [:td "N"] [:td (:n stats)]]
      [:tr [:td "N(Gained)"] [:td (:n-gained stats)]]
      [:tr [:td "N(Lost)"] [:td (:n-lost stats)]]]]]))

(defn adjust-win-lose [state changed-key]
  (let [win+lose (+ (:win-prob state) (:lose-prob state))
        adjust-down (max (- win+lose 100) 0)]
    (case changed-key
      :win-prob (update state :lose-prob - adjust-down)
      :lose-prob (update state :win-prob - adjust-down)
      state)))

(defn adjust-ruin [state]
  (let [win+lose (+ (:win-prob state) (:lose-prob state))]
    (assoc state :ruin-prob (.round js/Math (- 100 win+lose)))))

(defn model-parameter [s label k props]
  (let [v (get s k)]
    [ni/draggable-input
     (merge
       {:label label
        :value v
        :swap-value (fn [f] (swap! state #(-> % (update k f) (adjust-win-lose k) adjust-ruin)))
        :min 0
        :max 100
        :text-width 115
        :width 50}
       props)
     v]))

(defn model-controls [s]
  [:div
   [:p {:style {:font-size "0.75em"}} "Model controls"]
   [model-parameter s "claim price" :claim-price {}]
   [model-parameter s "win value" :win-value {:max 500}]
   [model-parameter s "lose value" :lose-value {}]
   [model-parameter s "ruin value" :ruin-value {}]
   [model-parameter s "win probability" :win-prob {}]
   [model-parameter s "lose probability" :lose-prob {}]
   [model-parameter s "ruin probability" :ruin-prob {:disabled? true :swap-value (constantly nil)}]])

(defn simulation-controls [s]
  [:div
   [:p {:style {:font-size "0.75em"}} "Simulation controls"]
   [model-parameter s "bet size" :bet-size {}]
   [model-parameter s "portfolios" :portfolios {:min 1 :max 1000}]
   [model-parameter s "bets" :bets {:min 10 :max 1000}]])

(defn app []
  (let [{:keys [claim-price win-prob lose-prob ruin-prob win-value lose-value ruin-value portfolios bets bet-size]
         :as s} @state]
    [:div {:style {:max-width 800}}
     [:div {:style {:display :flex :flex-wrap :wrap :align-items :baseline :justify-content :space-around}}
      [model-controls s]
      [simulation-controls s]]
     [simulation s]
     [:p "Simulation of " portfolios " portfolios betting " [:strong bet-size "%"]
      " of their bankroll on each of " bets " bets."]
     [:p "Each claim costs "  [:strong claim-price "¢"] " and has a "
      [:strong win-prob "%"] " chance of resolving to " [:strong win-value "¢, "]
      "a " [:strong lose-prob "%"] " chance of resolving to " [:strong lose-value "¢, "]
      "and a " [:strong ruin-prob "%"] " chance of resolving to " [:strong ruin-value "¢."]]]))


(defn stop []
  (js/console.log "Stopping..."))

(defn start []
  (js/console.log "Starting...")
  (render [app] (.getElementById js/document "app")))

(defn ^:export init []
  (start))
