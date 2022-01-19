;Matthew Sirkot
;CMPSC 460 Homework Assignment 1

;1.
(define (GCD u v)
  (if(= v 0) u (GCD v (mod u v)))
  
)

;2.
(define (power a b)
  (if(= b 1) a (power (* a a) (- b 1)))
)

;3.
(define (binom n k)
  (cond((< n 1) 0)
       ((= k 0) 1)
       ((= k n) 1)
       (else (+ (binom (- n 1) (- k 1)) (binom (- n 1) k))))
)

;4.
(define (getn l n)
  (cond((= n 0) (car l))
       (else (getn (cdr l) (- n 1))))
)

;5.
(define (getsmallest l)
  (define (getsmallesthelper l n)
      (cond((= (length l) 0) n)
         ((< (car l) n) (getsmallesthelper (cdr l) (car l)))
        (else (getsmallesthelper (cdr l) n)))
  )
  (getsmallesthelper l (car l))
)

;6.
(define (intersection l1 l2)
  (define (intersectionhelper l1 l2 l3)
    (cond((or (null? l1) (null? l2)) l3)
    ((not(member (car l2) l1)) (intersectionhelper l1 (cdr l2) l3))
    (else (intersectionhelper (member (car l2) l1) (cdr l2) (append l3 (list (car (member (car l2) l1)))))))
  )
  (intersectionhelper l1 l2 '())
)

;7.
(define (setdiff l1 l2)
  (define (setdiffhelper l1 l2 l3)
    (cond((null? l1) l3)
    ((not (member (car l1) l2)) (setdiffhelper (cdr l1) l2 (append l3 (list (car l1)))))
    (else (setdiffhelper (cdr l1) l2 l3)))
  )
  (setdiffhelper l1 l2 '())
)

;8.
(define (squarelist l1)
  (define (squarelisthelper l1 l2)
    (cond((null? l1) l2)
    (else (squarelisthelper (cdr l1) (append l2 (list (* (car l1) (car l1))))))) 
  )
  (squarelisthelper l1 '())
)
