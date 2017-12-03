(define input 289326)

(define (square x)
  (* x x))

(define (isqrt n)
  (define (isqrt_ x n)
    (let ((x2 (quotient (+ x (quotient n x))
			2)))
      (cond ((>= x2 x) x)
	    (else (isqrt_ x2 n)))))
  (isqrt_ n n))

(define (spiral-square-size number)
  (if (<= number 0)
      0
      (let* ((root (isqrt number))
	     (start (+ root
		       (if (even? root)
			   -1
			   0))))
	(do ((i start (+ i 2)))
	    ((>= (square i)
		 number) i)))))


(define (spiral-square-size2 number)
  (if (<= number 0)
      0
      (do ((i 1 (+ i 2)))
	  ((>= (square i)
	       number) i))))

(define (distance-spiral number)
  (let* ((size (spiral-square-size number))
	 (diff (1- size))
	 (radius (quotient diff
			   2))
	 (displacement-along-rim
	  (abs (- (remainder (1- number)
			     diff)
		  radius))))
    (+ displacement-along-rim
       radius)))

(define (distance-spiral2 number)
  (let* ((size (spiral-square-size2 number))
	 (diff (1- size))
	 (radius (quotient diff
			   2))
	 (displacement-along-rim
	  (abs (- (remainder (1- number)
			     diff)
		  radius))))
    (+ displacement-along-rim
       radius)))

(define (test number)
  (let* ((size (spiral-square-size number))
	 (diff (1- size))
	 (num-bottom-right (square size))
	 (num-bottom-left (- num-bottom-right diff))
	 (num-top-left (- num-bottom-left diff))
	 (num-top-right (- num-top-left diff)))
    (cond ((> number num-bottom-right) (error "too large"))
	  ((= number num-bottom-right) 'bot-right-corner)
	  ((> number num-bottom-left) 'bot)
	  ((= number num-bottom-left) 'bot-left-corner)
	  ((> number num-top-left) 'left)
	  ((= number num-top-left) 'top-left-corner)
	  ((> number num-top-right) 'top)
	  ((= number num-top-right) 'top-right-corner)
	  ((> number (- num-top-right diff)) 'right)
	  (else (error "too small")))))

(define (test g) (do ((i 1 (1+ i)))
		     ((> i g))
		   (distance-spiral (expt 10 17))))
(define (test2 g) (do ((i 1 (1+ i)))
		      ((> i g))
		    (distance-spiral2 (expt 10 17))))

(define (test-solution g) (do ((i 1 (1+ i)))
			      ((> i g))
			    (distance-spiral input)))
(define solution1 (distance-spiral input))
