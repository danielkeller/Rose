; Transform utilities
(define get-position car)
(define get-rotation cadr)
(define get-scale caddr)
(define (with-position xfm pos) `(,pos ,(cadr xfm) ,(caddr xfm)))
(define (with-rotation xfm rot) `(,(car xfm) ,rot  ,(caddr xfm)))
(define (with-scale xfm scl)    `(,(car xfm) ,(cadr xfm)  ,scl)))
;lenses could be implemented here without much difficulty