(load "linkedlist.scm")
(use test)

(test "linked-list-insert 1 '()" '(1) (linked-list-insert 1 '()))
(test "linked-list-insert 2 '(1)" '(2 1) (linked-list-insert 2 '(1)))
(test "linked-list-insert 1 '(1 2)" '(1 1 2) (linked-list-insert 1 '(1 2)))

(test "linked-list-append 1 '()" '(1) (linked-list-append 1 '()))
(test "linked-list-append 2 '(1)" '(1 2) (linked-list-append 2 '(1)))
(test "linked-list-append 1 '(1 2)" '(1 2 1) (linked-list-append 1 '(1 2)))

(test "linked-list-search 1 '(1 2 3 4)" '(1 2 3 4) (linked-list-search 1 '(1 2 3 4)))
(test "linked-list-search 2 '(1 2 3 4)" '(2 3 4) (linked-list-search 2 '(1 2 3 4)))
(test "linked-list-search 3 '(1 2 3 4)" '(3 4) (linked-list-search 3 '(1 2 3 4)))
(test "linked-list-search 4 '(1 2 3 4)" '(4) (linked-list-search 4 '(1 2 3 4)))
(test "linked-list-search 0 '(1 2 3 4)" '() (linked-list-search 0 '(1 2 3 4)))

(test "linked-list-delete 1 '(1 2 3 4)" '(2 3 4) (linked-list-delete 1 '(1 2 3 4)))
(test "linked-list-delete 2 '(1 2 3 4)" '(1 3 4) (linked-list-delete 2 '(1 2 3 4)))
(test "linked-list-delete 3 '(1 2 3 4)" '(1 2 4) (linked-list-delete 3 '(1 2 3 4)))
(test "linked-list-delete 4 '(1 2 3 4)" '(1 2 3) (linked-list-delete 4 '(1 2 3 4)))
(test "linked-list-delete 0 '(1 2 3 4)" '(1 2 3 4) (linked-list-delete 0 '(1 2 3 4)))
