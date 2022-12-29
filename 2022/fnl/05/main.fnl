(local inspect (require :inspect))

(λ expect [err value]
  (match value
    nil (error err)
    val val))

(local filename (expect "missing input filename" (?. arg 1)))

(λ split-empty-line [lines]
  (do
    (var lhs [])
    (var rhs [])
    (var flag false)
    (each [line lines]
      (do
        (if (= line "")
            (set flag true)
            (if flag (table.insert rhs line) (table.insert lhs line)))))
    [lhs rhs]))

(λ tok-row [line]
  (do
    (var out [])
    (for [i 1 (length line) 4]
      (table.insert out (string.sub line i (+ i 3))))
    out))

(λ skip-item? [item]
  (or (string.match item "^%s+$") (string.match item " %d ")))

(λ build-stacks [n lines]
  (do
    (var stacks (let [out []]
                  (for [i 1 n]
                    (table.insert out []))
                  out))
    (each [_ line (ipairs lines)]
      (each [j item (ipairs line)]
        (if (not (skip-item? item))
            (table.insert (. stacks j) 1 (string.sub item 2 2)))))
    stacks))

(λ parse-stacks [lines]
  (do
    (var tokens [])
    (each [_ line (ipairs lines)]
      (table.insert tokens (tok-row line)))
    (var n (length (. tokens 1)))
    (build-stacks n tokens)))

(fn fmap [f xs]
  (icollect [_ x (ipairs xs)]
    (f x)))

(λ parse-command [line]
  (match (string.match line "move (%d+) from (%d+) to (%d+)")
    (a b c) (fmap tonumber [a b c])
    any (error "invalid command")))

(λ exec-cmd [cmd stacks]
  (let [[n from to] cmd]
    (for [i 1 n]
      (do
        (var item (table.remove (. stacks from)))
        (table.insert (. stacks to) item)))))

(λ exec-cmd-v2 [cmd stacks]
  (let [[n from to] cmd]
    (var tmp [])
    (for [i 1 n]
      (do
        (var item (table.remove (. stacks from)))
        (table.insert tmp item)))
    (for [i 1 n]
      (do
        (var item (table.remove tmp))
        (table.insert (. stacks to) item)))))

(λ run-prog [cmds stacks exec-fn]
  (each [_ cmd (ipairs cmds)]
    (exec-fn cmd stacks)))

(λ empty? [arr]
  (if (= (length arr) 0) true false))

(λ last-elem [stack]
  (if (empty? stack) "" (. stack (length stack))))

(λ print-code [stacks]
  (do
    (var out "")
    (each [_ stack (ipairs stacks)]
      (set out (.. out (last-elem stack))))
    (print out)))

(let [lines (io.lines filename)]
  (let [[lhs rhs] (split-empty-line lines)
        stacks (parse-stacks lhs)
        stacks-v2 (parse-stacks lhs)
        cmds (fmap parse-command rhs)]
    (do
      (run-prog cmds stacks exec-cmd)
      (run-prog cmds stacks-v2 exec-cmd-v2)
      (print-code stacks)
      (print-code stacks-v2))))
