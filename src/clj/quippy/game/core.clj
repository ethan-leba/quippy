(ns quippy.game.core)

(def default-game-state
  {:state :lobby
   :players {}
   :rounds nil
   :votes nil
   :score nil})

(def players-per-prompt 3)

(defmulti process-user-event
  "Applies a user event to the game state along with a list of messages to send"
  (fn [game-state event player]
    [(:action event) (:state game-state)]))

(defmethod process-user-event :default [game-state _ _] [game-state {}])

(defmethod process-user-event
  [:lobby-join :lobby]
  [game-state {:keys [username]} player]
  (let [new-state (assoc-in game-state [:players player] username)]
    [new-state (into {} (for [player-id (-> new-state :players keys)]
                          [player-id {:players (-> new-state :players vals)}]))]))

(defn make-rounds! "Generates the ordering of rounds randomly"
  [players players-per-prompt]
  (into {} (for [player (keys players)
                 :let [quippers (-> players (dissoc player) keys)]]
             [player
              {:prompt nil
               :quips (into {} (->> quippers
                                    shuffle
                                    (take players-per-prompt)
                                    (map #(vector % nil))))}])))

(defmethod process-user-event
  [:game-start :lobby]
  [game-state _ player]
  (let [new-state (-> game-state
                      (assoc :state :prompt)
                      (assoc :rounds (make-rounds! (:players game-state) players-per-prompt))
                      (assoc :score (-> (:players game-state)
                                        (map #(vector % 0))
                                        (into {}))))]
    [new-state (into {} (for [player-id (-> new-state :players keys)]
                          [player-id {:state :prompt}]))]))

(defmethod process-user-event
  [:submit-prompt :prompt]
  [game-state {:keys [prompt user]} player])

(defmethod process-user-event
  [:submit-quip :quip]
  [game-state {:keys [quip user]} player])

(defmethod process-user-event
  [:submit-vote :voting]
  [game-state {:keys [quip user]} player])
