#!r6rs

;; Copyright (C) Marc Nieper-Wißkirchen (2021).  All Rights Reserved.

;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

(import (rnrs (6))
	(control-features threading))

(assert (%run
	 (lambda ()
	   (%current-dynamic-environment 42)
	   (eqv? (%current-dynamic-environment) 42))))

(assert (let ([x1 #f] [x2 #f])
	  (%run
	   (lambda ()
	     (%current-dynamic-environment 'thread1)
	     (let ([t (%thread-start!
		       (lambda ()
			 (%current-dynamic-environment 'thread2)
			 (set! x2 (%current-dynamic-environment))))])
	       (set! x1 (%current-dynamic-environment))
	       (%thread-join! t))))
	  (equal? '(thread1 thread2) (list x1 x2))))

(assert (let ([x #f])
	  (%run
	   (lambda ()
	     (let ([t (%thread-start!
			(lambda ()
			  (set! x (%current-thread))))])
	       (%thread-join! t)
	       (and (eq? x t)
		    (not (eq? x (%current-thread)))))))))

(assert (%run
	 (lambda ()
	   (let ([x #f]
		 [mtx (make-%mutex)]
		 [cv (make-%condition-variable)])
	     (%mutex-lock! mtx)
	     (let ([t (%thread-start!
		       (lambda ()
			 (set! x 41)
			 (%mutex-lock! mtx)
			 (%condition-variable-broadcast! cv)
			 (%mutex-unlock! mtx)))])
	       (%mutex-unlock! mtx cv)
	       (eq? x 41))))))

(assert (%run
	 (lambda ()
	   (let* ([signal? #f]
		  [t (%thread-start!
		      (lambda ()
			(set! signal? #t)
			(do () (#f)
			  (%thread-yield!))))])
	     (do () (signal?)
	       (%thread-yield!))
	     (%thread-terminate! t)
	     (%thread-join! t)
	     #t))))


;; Local Variables:
;; mode: scheme
;; End:
