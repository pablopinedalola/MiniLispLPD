(let* ((x 2) (y (+ x 3))) (* x y))
; Con let* y desazúcar: (let x = 2 in (let y = (+ x 3) in (* x y)))
; Esperado (eval): 10
