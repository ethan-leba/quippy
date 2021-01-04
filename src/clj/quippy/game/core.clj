(ns quippy.game.core
  (:require [clojure.set :as set]))

(def default-game-state
  {:state :lobby
   :players {}
   :prompts nil
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

;; TODO: Equal quips per player
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
                      (assoc :prompts (make-rounds! (:players game-state) players-per-prompt))
                      (assoc :score (->> game-state
                                        :players
                                        keys
                                        (map #(vector % 0))
                                        (into {}))))]
    [new-state (into {} (for [player-id (-> new-state :players keys)]
                          [player-id {:state :prompt}]))]))

(defn extract-prompts
  "Retrieves the prompts that a player must quip on"
  [{:keys [prompts]} player]
  (for [[_ {:keys [prompt quips]}] prompts
        :when (contains? quips player)]
    prompt))

(defmethod process-user-event
  [:submit-prompt :prompt]
  [game-state {:keys [prompt]} player]
  (let [new-state (assoc-in game-state [:prompts player :prompt] prompt)]
    (if-not (every? :prompt (-> new-state :prompts vals))
      [new-state {}]
      [(assoc new-state :state :quip)
       (into {} (for [player-id (-> new-state :players keys)]
         [player-id {:state :quip
                     :prompts (extract-prompts new-state player-id)}]))])))

(defn next-voting-round
  "Generates the next voting round if exists, or transitions to the final state"
  [{:keys [rounds] :as game-state}]
  ;; TODO: Handle no rounds case + clean up naming
  (if-not (seq rounds)
    [(assoc game-state :state :final)
     (into {} (for [player-id (-> game-state :players keys)]
                [player-id {:state :final
                            :scores (->> game-state
                                         :score
                                         (map (fn [[player score]] [((:players game-state) player) score]))
                                         (into {}))}]))]
    (let [[judge & rest-of-rounds] rounds
          selected-round (-> game-state :prompts judge)
          score-tracker (->> selected-round
                             :quips
                             (map (fn [[k _]] [k 0]))
                             (into {}))]
      [(-> game-state
           (assoc :rounds rest-of-rounds)
           (assoc :quip->player (-> selected-round :quips set/map-invert))
           (assoc :current-round judge)
           (assoc :votes store-tracker)
           (assoc :score (or (:score game-state) score-tracker)))
       (into {} (for [player-id (-> game-state :players keys)]
                  [player-id {:state :vote
                              :prompt (:prompt selected-round)
                              :quips (-> selected-round :quips vals)
                              :can-vote ((complement contains?) (:quips selected-round) player-id)}]))])))

(defmethod process-user-event
  [:submit-quip :quip]
  [game-state {:keys [quip prompt]} player]
  (let [intermediate-state (or (some->> (:prompts game-state)
                               (filter (fn [[k v]] (= (:prompt v) prompt)))
                               ;; First elem passing predicate, and then first of k-v tuple
                               first
                               first
                               (#(assoc-in game-state [:prompts % :quips player] quip)))
                      game-state)]
    (if-not (every? some? (->> intermediate-state :prompts vals (mapcat (comp vals :quips))))
      [intermediate-state {}]
      (-> intermediate-state
          (assoc :state :vote)
          (assoc :rounds (-> intermediate-state :players keys shuffle))
          next-voting-round))))

(defmethod process-user-event
  [:submit-vote :vote]
  [game-state {:keys [vote-for]} player]
  (let [new-state (-> game-state
                      (update-in [:votes (get (:quip->player game-state) vote-for)] inc)
                      (update-in [:score (get (:quip->player game-state) vote-for)] inc))]
    (if-not (= (-> game-state :players count (- players-per-prompt))
               (->> new-state :votes vals (reduce +)))
      [new-state {}]
      [new-state
       (into {} (for [player-id (-> new-state :players keys)]
                  [player-id {:state :results
                              :quips (->> (:quip->player new-state)
                                          (map (fn [[quip player]]
                                                 [quip {:quipper ((:players new-state) player)
                                                        :votes (get-in
                                                                new-state
                                                                [:votes
                                                                 ((:quip->player new-state) quip)])}]))
                                          (into {}))
                              :prompter (-> new-state :current-round)
                              :prompt (get-in new-state [:prompts (:current-round new-state) :prompt])}]))
       [next-voting-round 10]])))
