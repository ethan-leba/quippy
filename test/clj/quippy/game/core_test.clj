(ns quippy.game.core-test
  (:require
   [clojure.test :refer :all]
   [quippy.game.core :refer :all]))

(defn run-event [[game-state response-list] {:keys [event player]}]
  (let [[next-game-state responses] (process-user-event game-state event player)]
    [next-game-state (cons responses response-list)]))

(defn simulate-events [initial-state & [events]]
  (reduce run-event [initial-state '()] events))

(simulate-events default-game-state)

(defn repeat-map [keys val]
  (into {} (for [key keys] [key val])))

(def players (map (juxt (comp keyword str) str) "abcdefg"))

(deftest test-process-user-event
  (testing "invalid action"
    (let [[_ response-list]
          (simulate-events
           default-game-state
           {:event {:action nil}
            :player nil})]
      (is (= '({}) response-list))))
  (let [[lobby-state lobby-responses]
        (apply
         simulate-events
         default-game-state
         (for [[id username] players]
           {:event {:action :lobby-join
                    :username username}
            :player id}))
        [prompt-state start-responses]
        (simulate-events
         lobby-state
         {:event {:action :lobby-start}
          :player :a})
        [_ start-responses]
        (simulate-events
         lobby-state
         {:event {:action :prompt-submit
                  :prompts '("a" "b")}
          :player :a}
         {:event {:action :prompt-submit
                  :prompts '("c" "d")}
          :player :b}
         {:event {:action :prompt-submit
                  :prompts '("e" "f")}
          :player :c})]
    (testing "joining the lobby"
      (is (= (for [i (range (count players))
                   :let [in-lobby (take (inc i) players)]]
               (repeat-map (map first in-lobby)
                           {:state :lobby :players (map second in-lobby)}))
             lobby-responses)))
    (testing "starting the game"
      (is (= '((repeat-map [:a :b :c] {:state :prompt :number-of-prompts 2}))
             start-responses)))
    (testing "entering prompts"
      (is (= '({:a nil}
               {:b nil}
               {:c nil})
             start-responses))))) 

