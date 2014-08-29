(define (split n l)
    (define (help n l acc)
        (if (zero? n)
            `(,(reverse acc) ,(car l) ,(cdr l))
            (help (- n 1) (cdr l) (cons (car l) acc))))
    (help n l '()))

(define (element-lens n)
    (lambda (fmap f lst)
        (define splitted (split n lst))
        (fmap (lambda (v) `(,@(car splitted) ,v ,@(caddr splitted)))
            (f (cadr splitted)))))

(define (compose-lens outer inner)
    (lambda (fmap f lst)
        (outer fmap (lambda (lst1) (inner fmap f lst1)) lst)))

(define _0 (element-lens 0))
(define _1 (element-lens 1))
(define _2 (element-lens 2))
(define _3 (element-lens 3))
(define _4 (element-lens 4))

(define (const a) (lambda (b) a))
(define ($ f . as) (apply f as))

(define (set lens v struct)
    (lens $ (const v) struct))

(define (get lens struct)
    (lens (lambda (s v) v) id struct))

(define (change lens f struct)
    (lens $ f struct))

"lens.scm"