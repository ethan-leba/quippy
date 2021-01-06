(ns quippy.event)

(def default-game-state {:state :lobby
                         :server-state {}
                         :local-state {:registered false}})

(def local-states
  {:lobby {:registered false}
   :prompt {:submitted false}
   :vote {:voted false}})

(defmulti handle-server-event (fn [event _] (:event event)))

(defmethod handle-server-event :state-transition
  [event game-state]
  (let [new-state (-> game-state
                      (assoc :state (:state event))
                      (assoc :server-state (dissoc event :state :event)))]
    (if (= (:state game-state) (:state new-state))
      new-state
      (assoc new-state :local-state (get local-states (:state new-state) {})))))

(defmethod handle-server-event :reset-state
  [_ _]
  default-game-state)
