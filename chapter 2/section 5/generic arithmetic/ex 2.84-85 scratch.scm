(define SomeInt (make-scheme-number 9))
(define somerat (make-rational 3 4))
(define somereal (make-scheme-number 3.5))
(define somecomp (make-complex-from-real-imag 3 5))
(define projectablecomp (make-complex-from-real-imag 3 0))
(add someint somecomp)

somecomp
(project somecomp)
(define projected (project projectablecomp))
(level projected)
(real? projected)
(integer? projected)
(exact? projected)

somereal
(define projectedreal (project somereal))
projectedreal
(exact? projectedreal)
(integer? somereal)
(integer? projectedreal)
(project (make-scheme-number 42))

(define realtorat (rationalize (inexact->exact somereal) 1/10))
(denom realtorat)

somerat
(define zzz (project somerat))
(level zzz)
(type-tag zzz)

(define xzxz (make-complex-from-real-imag 10 0))
(define zxzx (make-complex-from-real-imag 14.5 0))
xzxz
zxzx
(project xzxz)
(project zxzx)
(project (project xzxz))
(raise (raise (raise (project (project (project xzxz))))))

(real? (project xzxz)) ;; #t
(integer? (project xzxz)) ;; #t
(exact? (project xzxz)) ;; #f
(inexact? (project xzxz)) ;; #t
(level (project xzxz)) ;; type integer

(real? (project zxzx)) ;; #t
(integer? (project zxzx)) ;; #f
(exact? (project zxzx)) ;; #f
(inexact? (project zxzx)) ;; #t
(level (project zxzx)) ;; type real

(project (make-rational 4 5))
