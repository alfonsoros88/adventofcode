(local filename (. arg 1))
(local input (with-open [f (io.open filename :r)]
               (f:read)))

(λ last-n-char-different? [n i string]
  (var win {})
  (for [c (- (+ i 1) n) i]
    (local char (string.sub string c c))
    (if (?. win char)
        (lua "return false")
        (tset win char true)))
  true)

(λ find-first-n-different [n input]
  (for [i n (length input)]
    (when (last-n-char-different? n i input)
      (lua "return i"))))

;; part1
(print (find-first-n-different 4 input))
;; part2
(print (find-first-n-different 14 input))
