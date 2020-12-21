(ns quippy.game.core)

(def default-game-state
  {:state :waiting
   :players #{}
   :rounds nil
   :prompt nil
   :quips nil
   :votes nil
   :score nil})

(defmulti process-user-event
  "Applies a user event to the game state along with a list of messages to send"
  (fn [game-state event]
    [(:action event) (:state game-state)]))

(defmethod process-user-event :default [game-state _] game-state)

(defmethod process-user-event
  [:game-start :lobby]
  [game-state event])

(defmethod process-user-event
  [:submit-prompt :prompt]
  [game-state {:keys [prompt user]}])

(defmethod process-user-event
  [:submit-quip :quip]
  [game-state {:keys [quip user]}])

(defmethod process-user-event
  [:submit-vote :voting]
  [game-state {:keys [quip user]}])
