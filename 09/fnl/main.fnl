(local raw-input (icollect [line (io.lines (. arg 1))]
                   line))

(local inspect (require :inspect))

(λ parse-line [line]
  (match (string.match line "([RULD]) (%d+)")
    (:R n) {:R (tonumber n)}
    (:L n) {:L (tonumber n)}
    (:U n) {:U (tonumber n)}
    (:D n) {:D (tonumber n)}))

(λ fmap [f list]
  (icollect [_ x (ipairs list)]
    (f x)))

(local input (fmap parse-line raw-input))

(λ adjust [x]
  (match x
    0 0
    (where x (> x 0)) 1
    (where x (< x 0)) -1))

(λ next-tail-pos [head tail]
  (let [{:x hx :y hy} head
        {:x tx :y ty} tail
        dx (- hx tx)
        dy (- hy ty)]
    (var new-x tx)
    (var new-y ty)
    (when (or (>= (math.abs dx) 2) (>= (math.abs dy) 2))
      (do
        (set new-x (+ new-x (adjust dx)))
        (set new-y (+ new-y (adjust dy)))))
    {:x new-x :y new-y}))

(λ inc [x]
  (+ x 1))

(λ dec [x]
  (- x 1))

(λ up [pos]
  (let [{: x : y} pos]
    {: x :y (inc y)}))

(λ down [pos]
  (let [{: x : y} pos]
    {: x :y (dec y)}))

(λ right [pos]
  (let [{: x : y} pos]
    {:x (inc x) : y}))

(λ left [pos]
  (let [{: x : y} pos]
    {:x (dec x) : y}))

(λ init-rope [size]
  (fcollect [i 1 size] {:x 0 :y 0}))

(λ process-command [positions command rope]
  (let [[n nxt] (match command
                  {:R n} [n up]
                  {:L n} [n down]
                  {:U n} [n right]
                  {:D n} [n left])
        nodes (length rope)]
    (var rope rope)
    (for [i 1 n]
      ;; update head
      (let [head (. rope 1)]
        (tset rope 1 (nxt head)))
      ;; update rope
      (for [j 2 nodes]
        (let [head (. rope (dec j))
              tail (. rope j)
              new-tail (next-tail-pos head tail)
              {: x : y} new-tail]
          (tset rope j new-tail)
          (when (= j nodes)
            (match (?. positions x)
              nil (tset positions x {y true})
              tbl (tset tbl y true))))))
    rope))

(λ run [cmds rope-size]
  (var positions {0 {0 true}})
  (var rope (init-rope rope-size))
  (each [_ cmd (ipairs cmds)]
    (let [new-rope (process-command positions cmd rope)]
      (set rope new-rope)))
  positions)

(λ count-values [positions]
  (var count 0)
  (each [k v (pairs positions)]
    (each [k2 _ (pairs v)]
      (set count (inc count))))
  count)

(local positions (run input 2))

(print (inspect (count-values positions)))

;; part2
(local positions-part2 (run input 10))
(print (inspect (count-values positions-part2)))
