#lang racket
(require r5rs)
(require rackunit)
(print-as-expression #f)
(print-mpair-curly-braces #f)

;find-assoc
(define (find-assoc key alist)
  (cond ((null? alist) #f)
	((equal? (caar alist) key) (cadar alist))
	(else (find-assoc key (cdr alist)))))

;add-assoc
(define (add-assoc key key2 val alist)
  (cons (list key key2 val) alist))

;make-point
(define (make-point x y)
  (cons x y))
;hashfunc
(define (hashfunc x y) void)

;bucket
(define (make-bucket tag) (list tag))

;make-vector
(define (make-vector size value)
  (make-bucket 'bucket))

;vector-ref
(define (vector-ref vector index key)
  (cond ((null? (cdr vector)) #f)
	((equal? (caar (cdr vector)) index)
         (if (equal? (cadr (car (cdr vector))) key) (car (cdr vector))
             (vector-ref (cdr vector) index key)))
	(else (vector-ref (cdr vector) index key))))

;vector-set!
(define (vector-set! vector index key key2 val)
  (if (list? vector)
      (set-cdr! vector (add-assoc key key2 val (cdr vector)))
      (error "error")))

;bucket-abstraction
(define (make-buckets N v) (make-vector N v))
(define bucket-ref vector-ref)
(define bucket-set! vector-set!)

;make-table
(define (make-table table-tag size hashfunc)
  (let ((buckets (make-buckets size null)))
    (list table-tag size hashfunc buckets)))

;table-element
(define (size-of tab) (cadr tab))
(define (hashfunc-of tab) (caddr tab))
(define (buckets-of tab) (cadddr tab))

;table-put!
(define (table-put! tab key key2 val)
  (let ((index ((hashfunc-of tab) key (size-of tab)))
        (buckets (buckets-of tab)))
    (bucket-set! buckets index key key2 val)))

;table-get
(define (table-get tab key key2)
  (let ((index ((hashfunc-of tab) key (size-of tab)))
        (buckets (buckets-of tab)))
    (bucket-ref buckets key key2)))

;global-table
(define global-table (make-table 'global-table 12 hashfunc))

;square
(define (square x) (* x x))

;attach-type
(define (attach-type type-tag contents)
  (cons type-tag contents))

;type
(define (type datum) (car datum))
(define (contents datum) (cdr datum))

;real, imag part
(define (real-part x) (car x))
(define (imag-part x) (cdr x))
(define (magnitude x)
  (sqrt (+ (square (real-part x)) (square (imag-part x)))))
(define (angle x)
  (atan (imag-part x) (real-part x)))
(define (make-from-real-imag x y) (cons x y))
(define (make-from-mag-ang r a)
  (cons (* r (cos a)) (* r (sin a))))

;rectangle
(define (make-rectangular x y)
  (attach-type 'rectangular (cons x y)))
(define (real-part-rectangular x) (car x))
(define (imag-part-rectangular x) (cdr x))
(define (magnitude-rectangular x)
  (sqrt (+ (square (car x))
           (square (cdr x)))))
(define (angle-rectangular x)
  (atan (cdr x) (car x)))
(define (make-complex-rectangular x y) (attach-type 'complex (cons x y)))

;polar
(define (make-polar r a)
  (attach-type 'polar (cons r a)))
(define (real-part-polar x)
  (* (car x) (cos (cdr x))))
(define (imag-part-polar x )
  (* (car x) (sin (cdr x))))
(define (magnitude-polar x) (car x))
(define (angle-polar x) (cdr x))
(define (make-complex-polar r a)
  (attach-type 'complex (cons (real-part-polar (cons r a))
                              (imag-part-polar (cons r a)))))

;make func
(define (make-complex x) (attach-type 'complex x))
(define (makeration x y) (attach-type 'rational (cons x y)))
(define (make-scheme-number x) (attach-type 'scheme-number x))

;install-complex-package
(define (install-complex-package)
  (define (+complex x1 x2)
    (make-from-real-imag (+ (real-part x1) (real-part x2))
                         (+ (imag-part x1) (imag-part x2))))
  (define (*complex x1 x2)
    (make-from-mag-ang (* (magnitude x1) (magnitude x2))
                       (+ (angle x1) (angle x2))))
  
  (define (complex*rational x1 x2)
    (cons (makeration (* (real-part x1) (real-part x2)) (imag-part x2))
          (cons (makeration (* (imag-part x1) (real-part x2)) (imag-part x2)) '())))
  (define (complex+rational x1 x2)
    (cons (makeration (+ (* (imag-part x2) (real-part x1)) (real-part x2))
                (imag-part x2)) 
          (imag-part x1)))
  
  (define (complex*number x1 x2)
    (cons (* x2 (real-part x1)) (* x2 (imag-part x1))))
  (define (complex+number x1 x2)
    (cons (+ x2 (real-part x1)) (imag-part x1)))
  (define (tag x) (attach-type 'complex x))
  
  (put 'MUL '(complex rational)
       (lambda (x1 x2) (tag (complex*rational x1 x2)))) 
  (put 'ADD '(complex rational)
       (lambda (x1 x2) (tag (complex+rational x1 x2))))
  (put 'MUL '(complex scheme-number)
       (lambda (x1 x2) (tag (complex*number x1 x2))))
  (put 'ADD '(complex scheme-number)
       (lambda (x1 x2) (tag (complex+number x1 x2))))
  (put 'MUL '(complex complex)
       (lambda (x1 x2) (tag (*complex x1 x2))))
  (put 'ADD '(complex complex)
       (lambda (x1 x2) (tag (+complex x1 x2))))
  'done)

;install-scheme-number-package
(define (install-scheme-number-package)
  (define (tag x)
    (attach-type 'scheme-number x))  
  (put 'MUL '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'ADD '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  'done)

;install-rational-package
(define (install-rational-package)
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (makeration n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (makeration (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (makeration (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (rat+number x y)
    (makeration (+ (* y (denom x)) (numer x))
              (denom x)))
  (define (rat*number x y)
    (makeration (* y (numer x))
              (denom x)))
  (define (tag x) (attach-type 'rational x))
  (put 'MUL '(rational scheme-number)
       (lambda (x y) (tag (rat*number x y))))
  (put 'ADD '(rational scheme-number)
       (lambda (x y) (tag (rat+number x y))))
  (put 'MUL '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'ADD '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  'done)

;put, get
(define (put type label val)
  (table-put! global-table type label val))
(define (get key key2)
  (table-get global-table key key2))

;ADD, MUL
(define (ADD x y) (apply-generic 'ADD x y))
(define (MUL x y) (apply-generic 'MUL x y))

;apply-generic
(define (apply-generic label x y)
  (let* ((x-type (car x))
         (y-type (car y))
         (cal-type1 (list x-type y-type))
         (cal-type2 (list y-type x-type)))
    (if (get label cal-type1) ((caddr (get label cal-type1)) (cdr x) (cdr y))
        (if (get label cal-type2) ((caddr (get label cal-type2)) (cdr y) (cdr x))
            (error "ERROR")))))

;install-package         
(install-complex-package)
(install-rational-package)
(install-scheme-number-package)

;test-case
(define com1 (make-complex-rectangular 1 4))
(define com2 (make-complex-rectangular 2 4))
(define com3 (make-complex-polar 4 2))
(define com4 (make-complex-polar 5 3))
(define rat1 (makeration 2 1))
(define rat2 (makeration 1 3))
(define num1 (make-scheme-number 7))
(define num2 (make-scheme-number 3))
(define num3 (list'scheme 4))

(ADD com1 com2) ;복소수(직교)+복소수(직교) = 복소수
(ADD com1 com4) ;복소수(직교)+복소수(극) = 복소수
(MUL com1 com4) ;복소수(직교)*복소수(극) = 복소수
(MUL com3 com4) ;복소수(극)*복소수(직교) = 복소수
(ADD rat1 rat2) ;유리수 + 유리수 = 유리수
(MUL rat1 rat2) ;유리수 * 유리수 = 유리수
(ADD num1 num2) ;실수 + 실수 = 실수
(MUL num1 num2) ;실수 * 실수 = 실수
(ADD com1 rat1) ;복소수 + 유리수 = 복소수 -> 유리수 + 허수
(MUL com3 rat1) ;복소수 * 유리수 = 복소수 -> 유리수*(실수 + 허수)
(MUL com3 num2) ;복소수 * 실수 = 복소수 -> 실수*(실수 + 허수)
(MUL rat2 num2) ;유리수 + 실수 = 유리수
