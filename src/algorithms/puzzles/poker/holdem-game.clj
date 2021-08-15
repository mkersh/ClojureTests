(ns algorithms.puzzles.poker.holdem-game
  (:require
   [algorithms.puzzles.poker.cards :as cards]))

(defn make-player [pmap player]
  (assoc pmap player
         {:player player :cards [] :chips 50}))

(defn make-players-map [plist]
  (reduce make-player {} plist))


(defn new-game [players]
  {:flop []       ;; will have 3 cards after flop
   :turn nil      ;; will have turn card after fourth community card shown
   :river nil     ;; will have river card after fifth community card shown
   :burncards []  ;; burncards will be added here during flop, turn and river

   :players (make-players-map players)})

(defn get-player-map [game name]
  (get (:players game) name))

(defn update-player-map [game pmap]
  (let [name (:player pmap)
        new-players-map (assoc (:players game) name pmap)]
    (assoc game :players new-players-map)))

(defn add-burncard [game card]
  (let [burnscards (:burncards game)
        new-burnscards (conj burnscards card)]
    (assoc game :burncards new-burnscards)))

(defn player-add-card! [game-atom name newcard]
(let [game @game-atom
      pmap (get-player-map game name)
      player-cards (:cards pmap)
      updated-pmap (assoc pmap :cards (conj player-cards newcard))
      updated-game (update-player-map game updated-pmap)]
  (reset! game-atom updated-game))
)

(defn player-get-chips [game-atom name]
  (let [game @game-atom
        pmap (get-player-map game name)
        player-chips (:chips pmap)]
    player-chips))

(defn player-update-chips! [game-atom name chips-total]
  (let [game @game-atom
        pmap (get-player-map game name)
        updated-pmap (assoc pmap :chips chips-total)
        updated-game (update-player-map game updated-pmap)]
    (reset! game-atom updated-game)))

(defn perform-the-flop! [game-atom pack-atom ]
  (let [game0 @game-atom
        burncard (cards/deal-card! pack-atom)
        game (add-burncard game0 burncard)
        card1 (cards/deal-card! pack-atom)
        card2 (cards/deal-card! pack-atom)
        card3 (cards/deal-card! pack-atom)
        updated-game (assoc game :flop [card1 card2 card3])]
    (reset! game-atom updated-game)))

(defn perform-the-turn! [game-atom pack-atom]
  (let [game0 @game-atom
        burncard (cards/deal-card! pack-atom)
        game (add-burncard game0 burncard)
        card1 (cards/deal-card! pack-atom)
        updated-game (assoc game :turn card1)]
    (reset! game-atom updated-game)))

(defn perform-the-river! [game-atom pack-atom]
  (let [game0 @game-atom
        burncard (cards/deal-card! pack-atom)
        game (add-burncard game0 burncard)
        card1 (cards/deal-card! pack-atom)
        updated-game (assoc game :river card1)]
    (reset! game-atom updated-game)))

(defn deal-all-players! [game-atom pack-atom]
  (let [game @game-atom
        players (:players game)]

    ;; Deal the first card to each player
    ;; NOTE: not exactly sure why but the doall is needed to get all updates to happen
    ;;       something to do with for returning a lazySeq but I'm not sure exactly what is happening here
    (doall (for [player (keys players)]
             (player-add-card! game-atom player (cards/deal-card! pack-atom))))

    ;; Deal the second card to each player
    (doall (for [player (keys players)]
             (player-add-card! game-atom player (cards/deal-card! pack-atom))))))

(comment
  
  (def the-pack (atom (cards/suffle-pack)))
  (cards/deal-card! the-pack)
  (count @the-pack)

  (def the-game (atom (new-game ["mark" "sharon" "andy" "ellie"])))
 
 ;; The main stages of the game
 (deal-all-players! the-game the-pack)
 (perform-the-flop! the-game the-pack)
 (perform-the-turn! the-game the-pack)
 (perform-the-river! the-game the-pack)
 
 ;; Pevious older testing
 (player-add-card! the-game "ellie" (cards/deal-card! the-pack))
 (player-get-chips the-game "ellie")
 (player-update-chips! the-game "ellie" 7900)
 
 (get-player-map @the-game "mark")
 (update-player-map @the-game {:player "andy", :cards [], :chips 700})
 

 (:players @the-game)
(get (:players @the-game) "andy")

(make-player {} "mark")
(make-players-map ["mark"])

 ;;
  )