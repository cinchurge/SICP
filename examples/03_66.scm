;        c1      c2    c3    c4    c5
; r1  (1,1) | (1,2) (1,3) (1,4) (1,5)
; r2  -------------------------------
; r3        | (2,2) (2,3) (2,4) (2,5)
; r4        |       (3,3) (3,4) (3,5)
; r5        |             (4,4) (4,5)
; r6        |                   (5,5)

; row increment -> column increment (change to first row) -> go back to lowest column that hasn't reached the diagnol yet
