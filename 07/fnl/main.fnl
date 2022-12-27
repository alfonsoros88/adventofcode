(local inspect (require :inspect))

(local filename (. arg 1))
(local input (icollect [line (io.lines filename)]
               line))

(λ fmap [f xs]
  (icollect [_ x (ipairs xs)]
    (f x)))

(λ parse-command [line]
  {:cd (string.match line "$ cd (.*)")})

(λ parse-dir [line]
  {:dir (string.match line "dir (.*)")})

(λ parse-file [line]
  (match (string.match line "(%d+) (.*)")
    (size file) {: file :size (tonumber size)}))

(λ parse-line [line]
  (match line
    (where line (string.find line "^%$ cd ")) (parse-command line)
    (where line (string.find line "^%$ ls")) :ls
    (where line (string.find line "^dir ")) (parse-dir line)
    (where line (string.find line "^%d+ ")) (parse-file line)
    any (error (.. "unknown line: " line))))

(λ build-tree [items]
  (var cwd [])
  (var tree {})
  (λ dir_tbl []
    (var tbl tree)
    (each [_ p (ipairs cwd)]
      (when (not (. tbl p))
        (tset tbl p {}))
      (set tbl (. tbl p)))
    tbl)
  (λ new-entry [f]
    (match f
      {: file : size} (tset (dir_tbl) file size)
      {: dir} (tset (dir_tbl) dir {})))
  (each [_ item (ipairs items)]
    (match item
      {: cd} (match cd
               ".." (table.remove cwd)
               "/" (set cwd [])
               dir (table.insert cwd dir))
      entry (new-entry entry)))
  tree)

(λ directory-sizes [name tree]
  (var sizes {name 0})
  (each [d content (pairs tree)]
    (match content
      (where x (= (type x) :number)) (let [size (. sizes name)]
                                       (tset sizes name (+ size x)))
      dir (let [subsizes (directory-sizes d content)]
            (each [k v (pairs subsizes)]
              (local k (.. name "/" k))
              (tset sizes k v))
            (tset sizes name (+ (. sizes name) (. sizes (.. name "/" d)))))))
  sizes)

(λ sum-bellow [n list]
  (var sum 0)
  (each [k v (pairs list)]
    (if (<= v n)
        (set sum (+ sum v))))
  sum)

(local tree (build-tree (fmap parse-line input)))
(local sizes (directory-sizes :root tree))

(print (sum-bellow 100000 sizes))

;; part2
(local total-space 70000000)
(local unused (- total-space (. sizes :root)))
(local needed-space 30000000)
(local need-free (- needed-space unused))

;; sort sizes
(local sorted-sizes (let [elems (icollect [_ v (pairs sizes)]
                                  v)]
                      (table.sort elems)
                      elems))

(λ lower_bound [n tbj]
  (each [_ i (ipairs tbj)]
    (if (<= n i)
        (lua "return i"))))

(print (lower_bound need-free sorted-sizes))
