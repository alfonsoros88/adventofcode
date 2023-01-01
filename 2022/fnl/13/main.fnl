(local lpeg (require :lpeg))
(local inspect (require :inspect))

(local pair-grammar
       (let [space (^ (lpeg.P " ") 0)]
         (lpeg.P {1 :S
                  :S (lpeg.V :list)
                  :elem (+ (lpeg.V :number) (lpeg.V :list))
                  :list (lpeg.Ct (* (lpeg.P "[")
                                    (+ (* space (lpeg.V :elem) space
                                          (^ (* (lpeg.P ",") space
                                                (lpeg.V :elem) space)
                                             0))
                                       (lpeg.P ""))
                                    (lpeg.P "]")))
                  :number (/ (^ (lpeg.R :09) 1) tonumber)})))

(λ parse-line [line]
  (pair-grammar:match line))

(local msgs (icollect [line (io.lines (. arg 1))]
              (parse-line line)))

(λ cmp [l r]
  (match [l r]
    [[lh & ltail] [rh & rtail]] (let [res (cmp lh rh)]
                                  (if (= res :continue) (cmp ltail rtail) res))
    [[lh & ltail] []] false
    [[] [rh & rtail]] true
    [[] []] :continue
    [[& ls] r] (cmp ls [r])
    [l [& rs]] (cmp [l] rs)
    [l r] (if (< l r) true (if (> l r) false :continue))))

(λ sum-ordered [input]
  (var sum 0)
  (for [i 1 (length input) 2]
    (let [l (. input i)
          r (. input (+ i 1))
          idx (+ (// i 2) 1)]
      (when (cmp l r)
        (set sum (+ sum idx)))))
  sum)

(print (sum-ordered msgs))

;; part 2

(table.insert msgs [[2]])
(table.insert msgs [[6]])

(λ poor-mans-sort [list]
  (let [n (length list)]
    (var idx (fcollect [i 1 n] i))
    (for [i 1 n]
      (var min i)
      (for [j (+ i 1) n]
        (when (cmp (. list (. idx j)) (. list (. idx min)))
          (set min j)))
      (local tmp (. idx i))
      (tset idx i (. idx min))
      (tset idx min tmp))
    idx))

(local sorted (poor-mans-sort msgs))

(print (inspect sorted))

(λ find-decoder [sorted]
  (let [n (length sorted)
        c2 n
        c1 (- n 1)]
    (var decoder 1)
    (for [i 1 n]
      (when (= (. sorted i) c1)
        (set decoder (* decoder i)))
      (when (= (. sorted i) c2)
        (set decoder (* decoder i))))
    decoder))

(print (find-decoder sorted))
