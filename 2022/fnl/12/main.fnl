(local input-raw (icollect [line (io.lines (. arg 1))]
                   line))

(λ char-index [c]
  (let [abc :abcdefghijklmnopqrstuvwxyz
        (idx _) (string.find abc c)]
    idx))

(λ index-char [i]
  (let [abc :abcdefghijklmnopqrstuvwxyz]
    (string.sub abc i i)))

(λ letter-to-height [c]
  (let [abc :abcdefghijklmnopqrstuvwxyz]
    (match c
      :S (char-index :a)
      :E (char-index :z)
      c (char-index c))))

(λ parse-input [lines]
  (icollect [_ line (ipairs lines)]
    (do
      (var output [])
      (string.gsub line "." #(table.insert output (letter-to-height $1)))
      output)))

(local input (parse-input input-raw))

(local inspect (require :inspect))

(λ init-mat [x y e]
  (fcollect [i 1 x] (fcollect [j 1 y] e)))

(λ pop-min [pair-list]
  (var min 1)
  (for [i 1 (length pair-list)]
    (when (< (. pair-list i 1) (. pair-list min 1))
      (set min i)))
  (table.remove pair-list min))

(λ neighbors [mat x y]
  (let [h (length mat)
        w (length (. mat 1))]
    (var neighbors [])
    (each [_ [i j] (ipairs [[0 -1] [0 1] [-1 0] [1 0]])]
      (let [nx (+ x i)
            ny (+ y j)]
        (when (and (>= nx 1) (<= nx h) (>= ny 1) (<= ny w)
                   (<= (- (. mat nx ny) (. mat x y)) 1))
          (table.insert neighbors [nx ny]))))
    neighbors))

(local max-cost 99999999)
(fn dijkstra [mat start ...]
  (let [h (length mat)
        w (length (. mat 1))
        [sx sy] start]
    (var cost (init-mat h w max-cost))
    (var queue (icollect [_ [x y] (ipairs [start ...])]
                 [0 [x y]]))
    (each [_ [_ [x y]] (ipairs queue)]
      (tset cost x y 0))
    (while (> (length queue) 0)
      (let [[c [x y]] (pop-min queue)
            ns (neighbors mat x y)]
        (each [_ [nx ny] (ipairs ns)]
          (local new-cost (+ c 1))
          (when (< new-cost (. cost nx ny))
            (tset cost nx ny new-cost)
            (table.insert queue [new-cost [nx ny]])))))
    cost))

(λ find-letter [input letter]
  (for [i 1 (length input)]
    (match (string.find (. input i) letter)
      nil nil
      (j _) (lua "return {i, j}"))))

(local start (find-letter input-raw :S))
(local end (find-letter input-raw :E))
(local cost (dijkstra input start))

(λ print-mat [mat cost]
  (let [h (length cost)
        w (length (. cost 1))]
    (for [i 1 h]
      (var row "")
      (for [j 1 w]
        (if (= (. cost i j) max-cost)
            (set row (.. row (index-char (. mat i j))))
            (set row (.. row "."))))
      (print (inspect row)))))

(λ shortest-path [mat start end]
  (let [cost (dijkstra mat start)
        [ex ey] end]
    (. cost ex ey)))

(print (shortest-path input start end))

;; part2

(λ find-lowest-coords [mat]
  (let [h (length mat)
        w (length (. mat 1))]
    (var lowest-coords [])
    (for [i 1 h]
      (for [j 1 w]
        (when (= (. mat i j) 1)
          (table.insert lowest-coords [i j]))))
    lowest-coords))

(λ fmap [f list]
  (icollect [_ x (ipairs list)]
    (f x)))

(λ shortest-shortest-path [mat end]
  (let [starts (find-lowest-coords mat)
        costs (dijkstra mat (table.unpack starts))
        [ex ey] end]
    (. costs ex ey)))

(print (shortest-shortest-path input end))
