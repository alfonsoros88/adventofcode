(local raw-input (icollect [line (io.lines (. arg 1))]
                   line))

(local inspect (require :inspect))

(λ parse-line [line]
  (let [(x y a b) (string.match line
                                "Sensor at x=(-?%d+), y=(-?%d+): closest beacon is at x=(-?%d+), y=(-?%d+)")]
    {:S [(tonumber x) (tonumber y)] :B [(tonumber a) (tonumber b)]}))

(local input (icollect [_ line (ipairs raw-input)]
               (parse-line line)))

(λ manhattan [p1 p2]
  (let [[x1 y1] p1
        [x2 y2] p2]
    (+ (math.abs (- x1 x2)) (math.abs (- y1 y2)))))

(λ intersect-area [sensor yline]
  (let [{: S : B} sensor
        radius (manhattan S B)
        [x y] S
        ydiff (math.abs (- y yline))]
    (if (<= ydiff radius) [(- x (- radius ydiff)) (+ x (- radius ydiff))] nil)))

(λ cmp [p1 p2]
  (let [[x1 y1] p1
        [x2 y2] p2]
    (or (< x1 x2) (and (= x1 x2) (< y1 y2)))))

(λ flatten [segments]
  (var output [(. segments 1)])
  (for [i 2 (length segments)]
    (let [n (length output)
          lst (. output n)
          s (. segments i)
          [a b] lst
          [x y] s]
      (if (<= x b)
          (tset output n 2 (math.max b y))
          (table.insert output s))))
  output)

(λ covered [sensors row]
  (var segments (icollect [_ sensor (ipairs sensors)]
                  (intersect-area sensor row)))
  (table.sort segments cmp)
  (flatten segments))

(λ sum-segments [segments]
  (accumulate [sum 0 _ [x y] (ipairs segments)]
    (+ sum (- y x))))

(print (sum-segments (covered input 2000000)))

;; part 2

(λ trunc-segment [segment min max]
  (match segment
    (where [a b] (or (< b min) (> a max))) nil
    [a b] [(math.max a min) (math.min b max)]))

(λ trunc [segments l r]
  (icollect [_ s (ipairs segments)]
    (trunc-segment s l r)))

(λ find-uncovered-segments [sensors max]
  (for [i 1 max]
    (let [cov (trunc (covered sensors i) 0 max)
          sum (sum-segments cov)]
      (when (< sum max)
        (lua "return i, cov")))))

(λ find-uncovered [sensors max]
  (λ find-x [segments]
    (var x 0)
    (each [_ [a b] (ipairs segments)]
      (if (or (< x a) (> x b)) (lua "return x") (set x (+ b 1)))))
  (let [(y segments) (find-uncovered-segments sensors max)
        x (find-x segments)]
    [x y]))

(λ tuning-freq [point]
  (let [[x y] point]
    (+ (* 4000000 x) y)))

(print (tuning-freq (find-uncovered input 4000000)))
