(local inspect (require :inspect))

(λ expect [err value]
  (match value
    nil (error err)
    val val))

(local filename (expect "missing input filename" (?. arg 1)))

(λ read-input []
  (icollect [line (io.lines filename)]
    line))

(λ split-once [del str]
  (let [p (string.find str del)]
    [(string.sub str 1 (- p 1)) (string.sub str (+ p 1))]))

(λ fmap [f xs]
  (icollect [_ x (ipairs xs)]
    (f x)))

(λ compose [f g]
  (fn [...]
    (f (g ...))))

(λ parse-ranges [line]
  (->> line
       (split-once ",")
       (fmap (compose (partial fmap tonumber) (partial split-once "-")))))

(local input (expect "failed to read input" (fmap parse-ranges (read-input))))

(λ overlap [a b]
  (let [[a1 a2] a
        [b1 b2] b]
    (and (<= a1 b1) (<= b2 a2))))

(λ full-overlap [x y]
  (or (overlap x y) (overlap y x)))

(λ count [cond xs]
  (accumulate [sum 0 _ x (ipairs xs)]
    (+ sum (if (cond x) 1 0))))

(local part1 (count (fn [line]
                      (->> line (table.unpack) (full-overlap)))
                    input))

(print part1)

(λ some-overlap [a b]
  (let [[a1 a2] a
        [b1 b2] b]
    (or (and (<= a1 b2) (<= b1 a1)) (and (<= b1 a2) (<= a1 b1)))))

(local part2 (count (fn [line]
                      (->> line (table.unpack) (some-overlap)))
                    input))

(print part2)
