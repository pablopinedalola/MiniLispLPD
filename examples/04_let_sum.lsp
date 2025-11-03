(let ((x 1) (y 2)) (+ x y))
; Esperado (core): CLet ...
; Esperado (eval): 3
; Traza (trace):
;   (let x = 1 in (let y = 2 in (x + y)))
;   (let y = 2 in (1 + y))
;   (1 + 2)
;   3
