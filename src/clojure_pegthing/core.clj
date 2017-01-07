(ns clojure-pegthing.core
  (:gen-class))

(declare successful-move prompt-move game-over prompt-rows)

(defn tri*
  ([] (tri* 0 1))
  ([sum n]
   (let [new-sum (+ sum n)]
     (cons new-sum (lazy-seq (tri* new-sum (inc n)))))))

(def tri (tri*))

(defn triangular?
  [n]
  (= n (last (take-while #(>= % n) tri))))

(defn row-tri
  [n]
  (last (take n tri)))

(defn in-bounds?
  [max-pos & positions]
  (= max-pos (apply max max-pos positions)))

(defn row-num
  [pos]
  (inc (count (take-while #(> pos %) tri))))

(defn connect
  [board max-pos pos neighbor destination]
  (println "max-pos : " max-pos " pos : " pos " neighbor : " neighbor " destination : "  destination)
  (if (in-bounds? max-pos neighbor destination)
    (reduce (fn [new-board [p1 p2]] (assoc-in new-board [p1 :connections p2] neighbor))
            board
            [[pos destination] [destination pos]])
    board))

(defn connect-right
  [board max-pos pos]
  (let [neighbor (inc pos)
        destination (inc neighbor)]
    (if-not (or (triangular? neighbor) (triangular? destination))
      (connect board max-pos pos neighbor destination)
      board)))

(defn connect-down-left
  [board max-pos pos]
  (let [row (row-num pos)
        neighbor (+ pos row)
        destination (+ 1 neighbor row)]
    (connect board max-pos pos neighbor destination)))

(defn connect-down-right
  [board max-pos pos]
  (let [row (row-num pos)
        neighbor (+ 1 pos row)
        destination (+ 2 neighbor row)]
    (connect board max-pos pos neighbor destination)))

(defn add-pos
  [board max-pos pos]
  (println "Creating pegs - max pos : " max-pos " pos: " pos)
  (let [pegged-board (assoc-in board [pos :pegged] true)]
    (reduce (fn [new-board connector] (connector new-board max-pos pos))
            pegged-board
            [connect-right connect-down-left connect-down-right])))

(defn new-board
  [rows]
  (let [initial-board {:rows rows}
        max-pos (row-tri rows)]
    (reduce (fn [board pos] (println board) (add-pos board max-pos pos))
            initial-board
            (range 1 (inc max-pos)))))

(def alpha-start 97)
(def alpha-end 123)
(def letters (map (comp str char) (range alpha-start alpha-end)))
(def pos-chars 3)

(def ansi-styles
  {:red "[31m"
   :green "[32m"
   :blue "[34m"
   :reset "[0m"})

(defn ansi
  [style]
  (str \u001b (style ansi-styles)))

(defn colorize
  [text color]
  (str (ansi color) text (ansi :reset)))

(defn pegged?
  [board pos]
  (get-in board [pos :pegged]))

(defn valid-moves
  [board pos]
  (into {} (filter (fn [[destination jumped]]
                     (and (not (pegged? board destination)) (pegged? board jumped)))
                   (get-in board [pos :connections]))))

(defn valid-move?
  [board p1 p2]
  (get (valid-moves board p1) p2))

(defn remove-peg
  [board pos]
  (assoc-in board [pos :pegged] false))

(defn place-peg
  [board pos]
  (assoc-in board [pos :pegged] true))

(defn move-peg
  [board p1 p2]
  (place-peg (remove-peg board p2) p1))

(defn make-move
  [board p1 p2]
  (println "make-move : " p1 p2)
  (if-let [jumped (valid-move? board p1 p2)]
    (move-peg (remove-peg board jumped) p1 p2)))

(defn can-move?
  [board]
  (some (comp not-empty (partial valid-moves board))
        (map first (filter #(get (second %) :pegged) board))))

(defn render-pos
  [board pos]
  (str (nth letters (dec pos))
       (if (get-in board [pos :pegged])
         (colorize "0" :blue)
         (colorize "-" :red))))

(defn row-positions
  [row-num]
  (range (inc (or (row-tri (dec row-num)) 0))
         (inc (row-tri row-num))))

(defn row-padding
  [row-num rows]
  (let [pad-length (/ (* (- rows row-num) pos-chars) 2)]
    (apply str (take pad-length (repeat " ")))))

(defn character-as-string
  [string]
  (re-seq #"[a-zA-Z]" string))

(defn render-row
  [board row-num]
  (str (row-padding row-num (:rows board))
       (clojure.string/join " " (map (partial render-pos board) (row-positions row-num)))))

(defn print-board
  [board]
  (doseq [row-num (range 1 (inc (:rows board)))]
    (println (render-row board row-num))))

(defn letter->pos
  "FIXME: description"
  [letter]
  (inc (- (int (first letter)) alpha-start)))

(defn get-input
  "gets the input"
  ([] (get-input ""))
  ([default]
   (let [input (clojure.string/trim (read-line))]
     (if (empty? input)
       default
       (clojure.string/lower-case input)))))

(defn prompt-move
  [board]
  (print-board board)
  (println "What now? Enter two letters:")
  (let [input (map letter->pos (character-as-string (get-input)))]
    (if-let [new-board (make-move board (first input) (second input))]
      (successful-move new-board)
      (do
        (println "\nNot allowed. Cheater!\n")
        (prompt-move board)))))

(defn successful-move
  [board]
  (if (can-move? board)
    (prompt-move board)
    (game-over board)))

(defn game-over
  [board]
  (let [remaining-pegs (count (filter :pegged (vals board)))]
    (println "Yo out! Yo had " remaining-pegs " pegs left. Ha!")
    (print-board board)
    (println "Wanna give it another shot? [Y/N]")
    (let [input (get-input "y")]
      (if (= "y" input)
        (prompt-rows)
        (do
          (println "Ha! Pussy!")
          (System/exit 0))))))

(defn prompt-empty-peg
  [board]
  (println "Yo board:")
  (print-board board)
  (println "Remove which peg?")
  (prompt-move (remove-peg board (letter->pos (get-input "e")))))

(defn prompt-rows
  []
  (println "How many rows?")
  (let [row (Integer. (get-input 5))
        board (new-board row)]
    (prompt-empty-peg board)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Lets play peg")
  (prompt-rows))
