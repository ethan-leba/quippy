(ns quippy.core
  (:require [reagent.core :as reagent]
            [reagent.dom :as rdom]
            [quippy.websockets :as ws]))

(defonce messages (reagent/atom []))

(defn message-list []
 [:ul
  (for [[i message] (map-indexed vector @messages)]
    ^{:key i}
    [:li message])])

(defn message-input []
 (let [value (atom nil)]
   (fn []
     [:input.form-control
      {:type :text
       :placeholder "type in a message and press enter"
       :value @value
       :on-change #(reset! value (-> % .-target .-value))
       :on-key-down
       #(when (= (.-keyCode %) 13)
          (ws/send-transit-msg!
           {:message @value})
          (reset! value nil))}])))

(defn home-page []
 [:div.container
  [:div.row
   [:div.col-md-12
    [:h2 "Welcome to chat"]]]
  [:div.row
   [:div.col-sm-6
    [message-list]]]
  [:div.row
   [:div.col-sm-6
    [message-input]]]])

(defn update-messages! [{:keys [message]}]
  (swap! messages #(vec (take 10 (conj % message)))))

(defn mount-components []
 (rdom/render [#'home-page] (.getElementById js/document "app")))

(defn init! []
 (mount-components)
 (ws/make-websocket! (str "ws://" (.-host js/location) "/ws") update-messages!))
