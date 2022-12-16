(λ split-half [line]
  (let [len (length line)
        half (/ len 2)]
    {:fst (string.sub line 0 half) :snd (string.sub line half)}))

(λ string-to-set [string]
  (let [ret {}]
    (string.gsub string "." #(tset ret $1 true))
    ret))

(λ find-common-char [lhs rhs]
  (let [s (string-to-set lhs)]
    (var x "")
    (string.gsub rhs "." #(if (?. s $1) (set x $1) ""))
    x))

(λ char-value [char]
  (let [to-byte #(string.byte $1)
        v (to-byte char)
        a (to-byte :a)
        A (to-byte :A)]
    (if (>= v a)
        (- (+ v 1) a)
        (- (+ v 27) A))))

(local input (icollect [line (io.lines :input.txt)]
               line))

(local part1 (accumulate [sum 0 _ line (pairs input)]
               (let [{: fst : snd} (split-half line)]
                 (+ sum (char-value (find-common-char fst snd))))))

(print part1)

(λ find-common-char-triple [l c r]
  (let [l (string-to-set l)
        r (string-to-set r)]
    (var x "")
    (string.gsub c "." #(if (and (?. l $1) (?. r $1)) (set x $1) ""))
    x))

(local part2 (let []
               (var sum 0)
               (for [i 1 (length input) 3]
                 (let [l (. input i)
                       c (. input (+ i 1))
                       r (. input (+ i 2))
                       common (find-common-char-triple l c r)]
                   (set sum (+ sum (char-value common)))))
               sum))

(print part2)
