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
	(control-features primitives))

(assert (eqv? 1 (%call-with-current-continuation
		 (lambda (k)
		   (+ 2 (k 1))))))

(assert (%call-with-current-continuation
	 (lambda (k1)
	   (%call-with-current-continuation
	    (lambda (k2)
	      (%continuation=? k1 k2))))))

(assert (eqv? 10 (%case-lambda-box-ref (%case-lambda-box 10
					 [() #f])
				       #f)))

(assert (%call-with-current-continuation
	 (lambda (k1)
	   (%call-in-continuation k1
				 (lambda ()
				   (%call-with-current-continuation
				    (lambda (k2)
				      (%continuation=? k1 k2))))))))

;; Local Variables:
;; mode: scheme
;; End:
