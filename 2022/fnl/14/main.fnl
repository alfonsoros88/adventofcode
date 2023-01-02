(local inspect (require :inspect))

(local raw-input (icollect [line (io.lines (. arg 1))]
                   line))

(λ parse-line [line]
  (icollect [x y (string.gmatch line "(%d+),(%d+)")]
    {:x (tonumber x) :y (tonumber y)}))

(local input (icollect [_ line (ipairs raw-input)]
               (parse-line line)))

(λ bset [board x y e]
  (when (not (. board x))
    (tset board x []))
  (tset board x y e))

(λ init-board [lines]
  (var board [])
  (var maxy 0)
  (each [_ line (ipairs lines)]
    (for [i 2 (length line)]
      (let [{:x a1 :y b1} (. line (- i 1))
            {:x a2 :y b2} (. line i)]
        (local x1 (math.min a1 a2))
        (local x2 (math.max a1 a2))
        (local y1 (math.min b1 b2))
        (local y2 (math.max b1 b2))
        (when (> y2 maxy)
          (set maxy y2))
        (for [i x1 x2]
          (for [j y1 y2]
            (bset board i j "#"))))))
  (values board maxy))

(local (board max-depth) (init-board input))

(λ drop-sand [board x y opt]
  (match opt
    {: maxy} (when (>= y maxy)
               (lua "return nil"))
    {: bedrock} (do
                  (when (>= y bedrock)
                    (bset board x y "#")
                    (lua "return {x, y}"))
                  (when (?. board x y)
                    (lua "return nil"))))
  (let [down (?. board x (+ y 1))
        left (?. board (- x 1) (+ y 1))
        right (?. board (+ x 1) (+ y 1))]
    (match [down left right]
      [nil _ _] (drop-sand board x (+ y 1) opt)
      [_ nil _] (drop-sand board (- x 1) (+ y 1) opt)
      [_ _ nil] (drop-sand board (+ x 1) (+ y 1) opt)
      [_ _ _] (do
                (bset board x y :o)
                [x y]))))

(while (drop-sand board 500 0 {:maxy max-depth}))

(λ count-sand [board]
  (var count 0)
  (each [_ x (pairs board)]
    (each [_ y (pairs x)]
      (when (= y :o)
        (set count (+ count 1)))))
  count)

(print (count-sand board))

(while (drop-sand board 500 0 {:bedrock (+ max-depth 2)}))

(print (count-sand board))
