(ns quippy.core
  (:require [reagent.core :as reagent]
            [reagent.dom :as rdom]
            [quippy.websockets :as ws]
            [quippy.event :as evt]))

(defonce game-state (reagent/atom evt/default-game-state))

(defmulti display :state)

(defn prompt-with-button [on-click]
  (let [value (reagent/atom "")]
    (fn []
      [:form
       [:div.mb-3
        [:input.form-control
         {:type :text
          :value @value
          :on-change #(reset! value (-> % .-target .-value))}]]
       [:div.mb-3.d-flex.justify-content-center
        [:input.btn.btn-primary.mx-auto
         {:type :button
          :value "Submit"
          :on-click (fn []
                      (let [textbox-value @value]
                        (when-not (empty? textbox-value)
                          (reset! value "")
                          (on-click textbox-value))))}]]])))

(defmethod display :lobby [{:keys [local-state server-state]
                            {:keys [registered]} :local-state
                            {:keys [players]} :server-state}]
  (if-not registered
    [:div
     [:h1 "Enter username:"]
     [prompt-with-button
      (fn [username]
        (ws/send-transit-msg! {:action :lobby-join :username username})
        (swap! game-state assoc-in [:local-state :registered] true))]]
    [:div
     [:h2 "We're in the lobby broo!"]
     [:div
      (for [player players]
        [:p.my-1 player])]
     ;; TODO: remove hardcoding
     (let [can-start (>= (count players) 4)]
       [:input.btn.btn-primary
        {:type :button
         :value (if can-start "Start the game" "Need at least 4 players to start")
         :disabled (not can-start)
         :on-click #(ws/send-transit-msg! {:action :game-start})}])]))

(defmethod display :prompt [{:keys [local-state]
                             {:keys [submitted]} :local-state}]
  (if-not submitted
    [:div
     [:h2.fst-italic "write a funny prompt!"]
     [prompt-with-button
      (fn [prompt]
        (ws/send-transit-msg! {:action :submit-prompt :prompt prompt})
        (swap! game-state assoc-in [:local-state :submitted] true))]]
    [:h1 "pls wait patiently for the other players to submit."]))


(defmethod display :quip [{:keys [server-state]
                           {:keys [prompts]} :server-state}]
  (if-let [prompt (first prompts)]
    [:div
     [:h2.fst-italic.text-secondary "write a funny quip!"]
     [:h1 prompt]
     [prompt-with-button
      (fn [quip]
        (ws/send-transit-msg! {:action :submit-quip :prompt (-> @game-state
                                                                (get-in [:server-state :prompts])
                                                                first  ) :quip quip})
        (swap! game-state (fn [game-state]
                            (assoc-in game-state [:server-state :prompts]
                                      (-> game-state
                                          (get-in [:server-state :prompts])
                                          rest)))))]]
    [:h1 "pls wait patiently for the other players to submit."]))

 (defmethod display :vote [{:keys [server-state]
                           {:keys [prompt quips can-vote]} :server-state
                           {:keys [voted]} :local-state}]
  (if-not voted
    [:div
     [:h3.fst-italic.text-secondary
      (if can-vote "pick the best quip!" "you can't vote since you wrote one of the quips.")]
     [:h1 prompt]
     (for [quip quips]
       [:div.my-2
        [:input.btn
         {:type :button
          :value quip
          :class (if can-vote "btn-primary" "btn-secondary")
          :disabled (not can-vote)
          :on-click (fn []
                      (swap! game-state assoc-in [:local-state :voted] true)
                      (ws/send-transit-msg! {:action :submit-vote :vote-for quip}))}]])]
    [:h1 "pls wait patiently for the other players to submit."]))

(defmethod display :results [{:keys [server-state]
                              {:keys [prompt prompter quips]} :server-state}]
  [:div
   [:h1 prompt]
   [:p.text-secondary prompter]
   (for [[quip {:keys [quipper votes]}] quips]
     [:div.my-2
      [:h3 quip]
      [:p.text-secondary (str quipper " -- " votes " votes")]])])


(defmethod display :final [{:keys [server-state]
                            {:keys [scores]} :server-state}]
  [:div
   [:h1 "Final scores"]
   (for [[player score] (sort-by second > scores)]
     [:div.my-2
      [:h3 (str player " -- " score " total votes")]])])


(defmethod display :default [_]
  [:h1 "Unknown state..."])

(defn container []
  [:div.container.text-center.my-4
   [display @game-state]])

(defn mount-components []
  (rdom/render [#'container] (.getElementById js/document "app")))

(defn init! []
  (mount-components)
  (ws/make-websocket! (str "ws://" (.-host js/location) "/ws")
                      (fn [msg]
                        (println msg)
                        (compare-and-set! game-state
                                          @game-state
                                          (evt/handle-server-event msg @game-state)))))


