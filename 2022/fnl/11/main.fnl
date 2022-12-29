(local raw-input (icollect [line (io.lines (. arg 1))]
                   line))

(local inspect (require :inspect))
(local lpeg (require :lpeg))

(λ split-empty-line [lines]
  (var groups [])
  (var group [])
  (each [_ line (ipairs lines)]
    (if (= line "")
        (do
          (table.insert groups group)
          (set group []))
        (table.insert group line)))
  (table.insert groups group)
  groups)

(λ parse-worry-update-fn [line]
  (let [number (lpeg.C (* (^ (lpeg.P "-") -1) (^ (lpeg.R :09) 1)))
        expr (lpeg.P (* (lpeg.P "  Operation: new = old ")
                        (lpeg.C (lpeg.S "+*")) " "
                        (lpeg.C (+ (lpeg.P :old) number))))
        (op fac) (expr:match line)]
    (match [op fac]
      ["+" :old] #(+ $1 $1)
      ["*" :old] #(* $1 $1)
      ["+" num] (let [n (tonumber num)]
                  #(+ $1 n))
      ["*" num] (let [n (tonumber num)]
                  #(* $1 n)))))

(λ divisible-by? [x y]
  (= 0 (% x y)))

(λ parse-test-fn [monkey]
  (let [n (tonumber (string.match (. monkey 4) "Test: divisible by (%d+)"))
        a (tonumber (string.match (. monkey 5) "If true: throw to monkey (%d+)"))
        b (tonumber (string.match (. monkey 6)
                                  "If false: throw to monkey (%d+)"))]
    #(if (divisible-by? $1 n) a b)))

(λ parse-monkey [monkey]
  {:items (icollect [num (string.gmatch (. monkey 2) "(-?%d+)")]
            (tonumber num))
   :update (parse-worry-update-fn (. monkey 3))
   :div (tonumber (string.match (. monkey 4) "Test: divisible by (%d+)"))
   :test (parse-test-fn monkey)
   :inspected 0})

(λ fmap [f list]
  (icollect [_ item (ipairs list)]
    (f item)))

(local monkeys (fmap parse-monkey (split-empty-line raw-input)))
(local div-all (accumulate [div 1 _ m (ipairs monkeys)]
                 (* div (. m :div))))

(λ round [monkeys]
  (each [_ monkey (ipairs monkeys)]
    (let [items (. monkey :items)
          update (. monkey :update)
          test (. monkey :test)]
      (each [_ item (ipairs (. monkey :items))]
        (local item (math.floor (/ (update item) 3)))
        (local target-monkey (+ 1 (test item)))
        (tset monkey :inspected (+ 1 (. monkey :inspected)))
        (table.insert (. monkeys target-monkey :items) item))
      (tset monkey :items []))))

(λ monkey-business [monkeys]
  (var inspections (icollect [_ m (ipairs monkeys)]
                     (. m :inspected)))
  (table.sort inspections)
  (* (table.remove inspections) (table.remove inspections)))

(for [i 1 20]
  (round monkeys))

(print (monkey-business monkeys))

;; part2

(local monkeys-v2 (fmap parse-monkey (split-empty-line raw-input)))
(local div-all (accumulate [div 1 _ m (ipairs monkeys-v2)]
                 (* div (. m :div))))

(λ round-v2 [monkeys]
  (each [_ monkey (ipairs monkeys)]
    (let [items (. monkey :items)
          update (. monkey :update)
          test (. monkey :test)]
      (each [_ item (ipairs (. monkey :items))]
        (local item (% (update item) div-all))
        (local target-monkey (+ 1 (test item)))
        (tset monkey :inspected (+ 1 (. monkey :inspected)))
        (table.insert (. monkeys target-monkey :items) item))
      (tset monkey :items []))))

(for [i 1 10000]
  (round-v2 monkeys-v2))

(print (monkey-business monkeys-v2))
