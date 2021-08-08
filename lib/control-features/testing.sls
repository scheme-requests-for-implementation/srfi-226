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

(library (control-features testing)
  (export test-begin test-end test)
  (import (rnrs (6))
	  (control-features define-who)
	  (only (control-features) run))

  (define *count* 0)
  (define *fail* #f)

  (define test-begin
    (lambda (name)
      (display "# Starting test ")
      (display name)
      (newline)))

  (define test-end
    (lambda ()
      (display "1..")
      (display *count*)
      (newline)
      (when *fail*
	(exit #f))))

  (define-syntax/who test
    (lambda (stx)
      (syntax-case stx ()
	[(_ expected-expr test-expr)
	 #'(test #f expected-expr test-expr)]
	[(_ name expected-expr test-expr)
	 #'(let-values ([expected expected-expr]
			[result (run (lambda () test-expr))])
	     (do-test name expected result))]
	[_
	 (syntax-violation who "invalid syntax" stx)])))

  (define do-test
    (lambda (name expected result)
      (set! *count* (fx+ *count* 1))
      (if (equal? expected result)
	  (display "ok ")
	  (begin
	    (set! *fail* #t)
	    (display "not ok ")))
      (display *count*)
      (when name
	(display " - ")
	(display name))
      (newline))))

;; Local Variables:
;; mode: scheme
;; End:
