(local raw-input (icollect [line (io.lines (. arg 1))]
                   line))

(local inspect (require :inspect))

(λ to-mat [lines]
  (icollect [_ line (ipairs lines)]
    (fcollect [i 1 (length line)] (tonumber (string.sub line i i)))))

(local mat (to-mat raw-input))
(local H (length mat))
(local W (length (. mat 1)))

(λ init-mat [x y v]
  (fcollect [_ 1 x] (fcollect [_ 1 y] v)))

(λ set-border [tbl v]
  (do
    (local h (length tbl))
    (local w (length (. tbl 1)))
    (for [i 1 h]
      ;; first column
      (tset tbl i 1 v)
      ;; last column
      (tset tbl i w v))
    (for [i 2 (- w 1)]
      ;; first row
      (tset tbl 1 i v)
      ;; last row
      (tset tbl h i v))))

(λ visibility-pass [vis x y fx fy max]
  (if (or (< x 2) (< y 2) (> x H) (> y W)) (lua :return))
  (var current (. mat x y))
  (if (> current max) (tset vis x y true) (set current max))
  (visibility-pass vis (fx x) (fy y) fx fy current))

(λ inc [x]
  (+ x 1))

(λ dec [x]
  (- x 1))

(λ id [x]
  x)

(local visible-mat (do
                     (local vis (init-mat H W false))
                     (set-border vis true)
                     ;; horizontal pass
                     (for [i 2 (dec H)]
                       ;; forward
                       (visibility-pass vis i 2 id inc (. mat i 1))
                       ;; backward
                       (visibility-pass vis i (dec W) id dec (. mat i W)))
                     ;; vertical pass
                     (for [j 2 (dec W)]
                       ;; down
                       (visibility-pass vis 2 j inc id (. mat 1 j))
                       ;; up
                       (visibility-pass vis (dec H) j dec id (. mat H j)))
                     vis))

(λ count-visible [tbl]
  (local h (length tbl))
  (local w (length (. tbl 1)))
  (var count 0)
  (for [i 1 h]
    (for [j 1 w]
      (if (. tbl i j) (set count (+ count 1)))))
  count)

(print (count-visible visible-mat))

(λ scenic-score-pass [x y fx fy count max]
  (if (or (> x H) (> y W) (< x 1) (< y 1)) (lua "return count"))
  (local current (. mat x y))
  (if (>= current max) (lua "return count + 1"))
  (scenic-score-pass (fx x) (fy y) fx fy (inc count) max))

(λ scenic-score [x y]
  (local max (. mat x y))
  (* (scenic-score-pass (inc x) y inc id 0 max)
     (scenic-score-pass (dec x) y dec id 0 max)
     (scenic-score-pass x (inc y) id inc 0 max)
     (scenic-score-pass x (dec y) id dec 0 max)))

(λ max-scenic-score []
  (var max-score 0)
  (for [i 2 (- H 1)]
    (for [j 2 (- W 1)]
      (local score (scenic-score i j))
      (if (> score max-score) (set max-score score))))
  max-score)

(print (inspect (max-scenic-score)))
