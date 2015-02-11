(load "insertionsort.scm")
(use test)

(test "insert" '(1 4 2 3) (insert 1 '(4 2 3)))
(test "insertion-sort" '(1 2 3 4) (insertion-sort '(1 4 2 3)))
