(local input-raw (icollect [line (io.lines (. arg 1))]
                   line))

(local inspect (require :inspect))

(λ parse-addx [line]
  {:addx (tonumber (string.match line "^addx (-?%d+)$"))})

(λ parse-cmd [line]
  (match line
    (where line (string.find line :^noop)) :noop
    (where line (string.find line :^addx)) (parse-addx line)))

(λ fmap [f list]
  (icollect [_ i (ipairs list)]
    (f i)))

(local input (fmap parse-cmd input-raw))

(λ run [cycles cmd]
  (var X 1)
  (var cycle 1)
  (var output [])
  (var ip 1)
  (while (<= cycle cycles)
    (table.insert output X)
    (match (. cmd ip)
      :noop nil
      {: addx} (do
                 (table.insert output X)
                 (set cycle (+ cycle 1))
                 (set X (+ X addx))))
    (set cycle (+ cycle 1))
    (set ip (+ ip 1)))
  output)

(local cycle-values (run 240 input))
(local key-cycles [20 60 100 140 180 220])

(λ signal-strength [cycle xreg]
  (* cycle xreg))

(local part1 (accumulate [sum 0 _ i (ipairs key-cycles)]
               (+ sum (signal-strength i (. cycle-values i)))))

(print part1)

;; part 2
(λ render-row [start end]
  (local len (- end start))
  (var row "")
  (for [px 0 len]
    (local cycle (+ start px))
    (local cycle-val (. cycle-values cycle))
    (if (<= (math.abs (- px cycle-val)) 1)
        (set row (.. row "#"))
        (set row (.. row " "))))
  row)

(print (render-row 1 40))
(print (render-row 41 80))
(print (render-row 81 120))
(print (render-row 121 160))
(print (render-row 161 200))
(print (render-row 201 240))
