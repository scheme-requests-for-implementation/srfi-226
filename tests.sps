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

(import (except (rnrs (6))
                call-with-current-continuation
                call/cc
                dynamic-wind
                with-exception-handler
                guard
                current-input-port
                current-output-port
                current-error-port
                with-input-from-file
                with-output-to-file
                read-char
                peek-char
                raise raise-continuable
                read
                write-char
                newline
                display
                write)
        (prefix (rnrs (6)) rnrs:)
        (control-features)
        (control-features testing))

;;; Helpers

(define call-with-tail-test
  (let ([mark (make-continuation-mark-key 'tail)])
    (lambda (proc)
      (with-continuation-mark mark #t
        (proc (lambda ()
                (call-with-immediate-continuation-mark mark values)))))))

;;; Test Begin

(test-begin "Control Operators")

;;; Evaluation

(test 10 10)

(test (values 1 2) (values 1 2))

;;; Guard

;;; Continuation prompts

(define tag (make-continuation-prompt-tag 'tag))

(test #t (continuation-prompt-tag? tag))

(test 42 (guard (c [(continuation-violation? c) 42])
           (abort-current-continuation tag)))

(test 5 (call-with-continuation-prompt (lambda () 5)
                                        (make-continuation-prompt-tag)))

(test 1 (+ 1 (abort-current-continuation (default-continuation-prompt-tag)
                (lambda () 1))))

(test 6 (+ 2 (call-with-continuation-prompt
               (lambda ()
                 (+ 3 (abort-current-continuation (default-continuation-prompt-tag)
                        (lambda () 4))))
               (default-continuation-prompt-tag))))

(test 4 (+ 2 (call-with-continuation-prompt
               (lambda ()
                 (+ 3 (abort-current-continuation (default-continuation-prompt-tag)
                        (lambda () 4))))
               tag)))

(test 7 (+ 3 (call-with-continuation-prompt
               (lambda ()
                 (+ 13 (abort-current-continuation tag
                         (lambda () 4))))
               tag)))

(test 9 (+ 2 (call-with-continuation-prompt
               (lambda ()
                 (if (call-with-composable-continuation
                      (lambda (proc)
                        (abort-current-continuation (default-continuation-prompt-tag)
                                                    (lambda ()
                                                      (+ (proc #f) (proc #t))))))
                     3
                     4))
               (default-continuation-prompt-tag)
               (lambda (thunk)
                 (thunk)))))

(test 2112
      (guard (c [else c])
        (call-with-continuation-prompt
         (lambda ()
           (raise 2112)))))

;;; Current Continuation

(test 12 (+ 2 (call-with-current-continuation
                (lambda (k)
                  (+ 1 (k 10))))))

(test 15 (+ 2 (call-with-composable-continuation
                (lambda (k)
                  (+ 1 (k 10))))))

(test 13 (+ 3 (call-with-current-continuation
                (lambda (k)
                  (+ 1 (call-in-continuation k (lambda () 10)))))))

(test #t (call-with-current-continuation
           (lambda (k)
             (continuation? k))))

(test #t (call-with-composable-continuation
           (lambda (k)
             (continuation? k))))

(test #f (continuation-prompt-available? tag))

(test #t (continuation-prompt-available? (default-continuation-prompt-tag)))

(test 111 (call-with-current-continuation
           (lambda (k)
             (call-with-continuation-prompt
              (lambda ()
                (k 111))))))

;;; Continuation barriers

(test 103 (call-with-continuation-barrier
           (lambda ()
             (call/cc
              (lambda (k)
                (+ 100 (k 103)))))))

(test 104 (call/cc
           (lambda (k)
             (call-with-continuation-barrier
              (lambda ()
                (+ 100 (k 104)))))))

(test 112 (call-with-current-continuation
           (lambda (k)
             (call-with-continuation-barrier
              (lambda ()
                (call-with-continuation-prompt
                 (lambda ()
                   (k 112))))))))

;;; Continuation marks

(test 'mark1 (with-continuation-mark 'key 'mark1
                (call-with-immediate-continuation-mark 'key values)))

(test 'mark2 (with-continuation-mark 'key 'mark1
                (with-continuation-mark 'key 'mark2
                  (call-with-immediate-continuation-mark 'key values))))

(test '(#f) (with-continuation-mark 'key 'mark1
               (list
                (call-with-immediate-continuation-mark 'key values))))

(test '((mark1) (mark2))
       (with-continuation-mark 'key1 'mark1
         (with-continuation-mark 'key2 'mark2
           (list
            (continuation-mark-set->list #f 'key1)
            (continuation-mark-set->list #f 'key2)))))

(test '((mark1) (mark2))
      (with-continuation-marks (['key1 'mark1]
                                ['key2 'mark2])
        (list
          (continuation-mark-set->list #f 'key1)
          (continuation-mark-set->list #f 'key2))))

(test '(1)
      (let f ([n 10])
        (if (fxzero? n)
            (continuation-mark-set->list #f 'key)
            (with-continuation-mark 'key n
              (f (fx- n 1))))))

(test '(mark2)
      (with-continuation-mark 'key 'mark1
        (call-with-continuation-prompt
         (lambda ()
           (with-continuation-mark 'key 'mark2
             (continuation-mark-set->list #f 'key))))))

(test '(mark2)
      (with-continuation-mark 'key 'mark1
        (list
         (with-continuation-mark 'key 'mark2
           (continuation-mark-set-first #f 'key)))))

(test '(((#(#f mark2) #(mark1 mark2))))
      (with-continuation-mark 'key1 'mark1
        (with-continuation-mark 'key2 'mark2
          (list
           (with-continuation-mark 'key3 'mark3
             (list
              (with-continuation-mark 'key2 'mark2
                (continuation-mark-set->list* #f '(key1 key2)))))))))

;;; Dynamic-wind

(test '(11 111) (let ([x 0])
                  (list
                   (dynamic-wind
                       (lambda () (set! x (fx+ x 1)))
                       (lambda () (set! x (fx+ x 10)) x)
                       (lambda () (set! x (fx+ x 100))))
                   x)))

(test "in pre out in post out "
      (let-values ([(p get) (open-string-output-port)])
        (let ([v (call/cc
                  (lambda (out)
                    (dynamic-wind
                        (lambda () (put-string p "in "))
                        (lambda ()
                          (put-string p "pre ")
                          (put-string p (call/cc out))
                          #f)
                        (lambda () (put-string p "out ")))))])
          (when v (v "post ")))
        (get)))

(test "in out"
      (let-values ([(p get) (open-string-output-port)])
        (dynamic-wind
            (lambda () (put-string p "in "))
            (lambda () (abort-current-continuation (default-continuation-prompt-tag)
                         (lambda ()
                           (get))))
            (lambda () (put-string p "out")))))

;;; See: <https://docs.racket-lang.org/reference/cont.html>.
(test 'cancel-canceled
      (call/cc
       (lambda (k0)
         (call/cc
          (lambda (k1)
            (dynamic-wind
                (lambda () #f)
                (lambda () (k0 'cancel))
                (lambda () (k1 'cancel-canceled))))))))

;;; See: <https://docs.racket-lang.org/reference/cont.html>.
(test '((1 . 5) (2 . 6) (3 . 5) (1 . 5) (2 . 6) (3 . 5))
      (let* ([x (make-parameter 0)]
             [l '()]
             [void (lambda arg* #f)]
             [add! (lambda (a b)
                     (set! l (append l (list (cons a b)))))])
        (let ([k (parameterize ([x 5])
                   (dynamic-wind
                       (lambda () (add! 1 (x)))
                       (lambda () (parameterize ([x 6])
                                    (let ([k+e (call/cc (lambda (k) (cons k void)))])
                                      (add! 2 (x))
                                      ((cdr k+e))
                                      (car k+e))))
                       (lambda () (add! 3 (x)))))])
          (parameterize ([x 7])
            (call/cc
             (lambda (c)
               (k (cons void c))))))
        l))

(test '(in thread in out out)
      (let [(l '())]
        (define out!
          (lambda (x)
            (set! l (cons x l))))
        (thread-join!
         (thread-start!
          (thread
           (dynamic-wind
               (lambda ()
                 (out! 'in))
               (lambda ()
                 (call/cc
                  (lambda (k)
                    (thread-join!
                     (thread-start!
                      (thread
                       (out! 'thread)
                       (k)))))))
               (lambda ()
                 (out! 'out))))))
        (reverse l)))

;;; Parameters

(define param (make-parameter 10 (lambda (x) (* x x))))

(test 100 (param))

(param 12)

(test 144 (param))

(test 169 (parameterize ([param 13]) (param)))

(test 144 (param))

(test 64 (parameterize ([param 13]) (param 8) (param)))

(test 144 (param))

(test #t (call-with-tail-test
          (lambda (tail?)
            (parameterize ([param 13])
              (tail?)))))

(test #t (parameterization? (current-parameterization)))

(test '(196 144)
      (let ([ps
             (parameterize ([param 14]) (current-parameterization))])
        (list
         (call-with-parameterization ps
           (lambda ()
             (param)))
         (param))))

;;; Exception handlers

(test 45 (with-exception-handler
             (lambda (con)
               (abort-current-continuation (default-continuation-prompt-tag)
                 (lambda () con)))
           (lambda ()
             (raise 45))))

(test #t (call-with-tail-test
          (lambda (tail?)
            (with-exception-handler
                (lambda (con)
                  #f)
              (lambda ()
                (tail?))))))

(test "ok" (guard (c
                   [(non-continuable-violation? c) "ok"])
             (with-exception-handler
              (lambda (c)
                #f)
              (lambda ()
                (raise 42)))))

(test 992 (with-exception-handler
           (lambda (con)
             (fx+ 1 con))
           (lambda ()
             (raise-continuable 991))))

;;; Initial Continuations

(test #f (with-continuation-mark 'key 'mark
           (call-in-initial-continuation
            (lambda ()
              (continuation-mark-set-first #f 'key)))))

;;; Threads

(test 98 (let ([t (thread-start!
                   (thread 98))])
           (thread-join! t)))

(test 96 (let ([t (thread-start!
                   (thread (raise 97)))])
           (guard (c
                   [(uncaught-exception-error? c)
                    (fx+ -1 (uncaught-exception-error-reason c))])
             (thread-join! t))))

(test 10 (let ([p (make-parameter 9)])
           (parameterize ([p 10])
             (let ([t (thread-start!
                       (thread (p)))])
               (thread-join! t)))))

(test #t (let* ([signal? #f]
                [t (thread-start!
                    (thread
                     (set! signal? #t)
                     (do () (#f)
                       (thread-yield!))))])
           (do () (signal?)
             (thread-yield!))
           (thread-terminate! t)
           (guard (c
                   [(thread-already-terminated-error? c)])
             (thread-join! t)
             #f)))

(test 734 (let* ([p (make-parameter 734)]
                 [t (thread (p))])
            (parameterize ([p 735])
              (thread-join! (thread-start! t)))))

(test '(12 13) (let* ([k #f]
                      [t (thread-start!
                          (thread
                           (call-with-current-continuation
                            (lambda (c)
                              (set! k c)
                              12))))]
                      [x (thread-join! t)])
                 (k (list x 13))
                 14))

;;; Promises

(test 213 (force (delay 213)))

(test (values 3 4) (force (delay (values 3 4))))

(test 214 (force (make-promise 214)))

(test 100 (force (delay (force (delay 100)))))

(test 1 (let* ([x 0]
               [s (delay (set! x (fx+ x 1)))])
          (force s)
          (force s)
          x))

(test 1 (let ([x 0])
          (letrec ([r (delay (set! x (fx+ x 1)))]
                   [s (delay (force r))]
                   [t (delay (force s))])
            (force t)
            (force r)
            x)))

(test 1 (let* [(p (make-parameter 1))
               (s (delay (p)))]
          (parameterize ([p 2])
            (force s))))

(test '(0 10 1 10 1)
      (let* ([l '()]
             [x 0]
             [p (delay (set! x (fx+ x 1))
                       (raise 10))])
        (define out!
          (lambda (x)
            (set! l (cons x l))))
        (define get
          (lambda ()
            (reverse l)))
        (out! x)
        (out!
         (guard (c [(uncaught-exception-error? c)
                    (uncaught-exception-error-reason c)])
           (force p)
           3))
        (out! x)
        (out!
         (guard (c [(uncaught-exception-error? c)
                    (uncaught-exception-error-reason c)])
           (force p)
           4))
        (out! x)
        (get)))

(test 1000 (force (delay (abort-current-continuation (default-continuation-prompt-tag)
                           (lambda ()
                             1000)))))

;;; See: <https://srfi-email.schemers.org/srfi-39/msg/2784435/>.
(test '(once #f 1)
      (let ([l '()])
        (define out!
          (lambda (x)
            (set! l (cons x l))))
        (define get
          (lambda ()
            (reverse l)))
        (let* ([x (delay (call-with-current-continuation (lambda (k) (k 1))))]
               [_ (out! 'once)]
               [y (force x)])
          (out! (integer? x))
          (out! y))
        (get)))

;;; See: <https://srfi-email.schemers.org/srfi-39/msg/2784435/>.
(test '(1 2 3)
      (let* ()
        (define (foreach->lazy-list foreach-fn collection)
          (delay
            (call-with-current-continuation
             (lambda (k-main)
               (foreach-fn
                (lambda (val)
                  (call-with-current-continuation
                   (lambda (k-reenter)
                     (k-main (cons val
                                   (delay
                                     (call-with-current-continuation
                                      (lambda (k-new-main)
                                        (set! k-main k-new-main)
                                        (k-reenter #f)))))))))
                collection)
               (k-main '())))))
        (define lazy-list->list
          (lambda (lazy-list)
            (let ([ls (force lazy-list)])
              (if (pair? ls)
                  (cons (car ls) (lazy-list->list (cdr ls)))
                  '()))))
        (lazy-list->list (foreach->lazy-list for-each '(1 2 3)))))

;;; Examples from the specification

(test #t (continuation-prompt-tag? (default-continuation-prompt-tag)))
(test #t (eq? (default-continuation-prompt-tag) (default-continuation-prompt-tag)))
(test #t (continuation-prompt-tag? (make-continuation-prompt-tag)))
(test #f (equal? (make-continuation-prompt-tag) (default-continuation-prompt-tag)))
(test #f (equal? (make-continuation-prompt-tag) (make-continuation-prompt-tag)))

(test '(foo bar)
      (let ([tag (make-continuation-prompt-tag)])
        (call-with-continuation-prompt
         (lambda ()
           (+ 1
              (abort-current-continuation tag 'foo 'bar)
              2))
         tag
         list)))
(test 27
      (let ([tag (make-continuation-prompt-tag)])
        (call-with-continuation-prompt
         (lambda ()
           (abort-current-continuation tag
             (lambda ()
               (abort-current-continuation tag
                 (lambda ()
                   27)))))
         tag #f)))

(test 990
      (let ([tag (make-continuation-prompt-tag)])
        (* 2
           (call-with-continuation-prompt
            (lambda ()
              (* 3
                 (call-with-non-composable-continuation
                  (lambda (k)
                    (* 5
                       (call-with-continuation-prompt
                        (lambda ()
                          (* 7 (k 11)))
                        tag)))
                  tag)))
            tag))))

(test 6930
      (let ([tag (make-continuation-prompt-tag)])
        (* 2
           (call-with-continuation-prompt
            (lambda ()
              (* 3
                 (call-with-composable-continuation
                  (lambda (k)
                    (* 5
                       (call-with-continuation-prompt
                        (lambda ()
                          (* 7 (k 11)))
                        tag)))
                  tag)))
            tag))))
(test '(4 5 9 17 25)
      (let* ()
        (define-syntax reset
          (syntax-rules ()
            [(reset e1 e2 ...)
             (call-with-continuation-prompt
              (lambda ()
                e1 e2 ...))]))
        (define-syntax shift
          (syntax-rules ()
            [(shift k e1 e2 ...)
             (call-with-composable-continuation
              (lambda (k)
                (abort-current-continuation (default-continuation-prompt-tag)
                  (lambda ()
                    e1 e2 ...))))]))
        (list
         (+ 1 (reset 3))
         (+ 1 (reset (* 2 (shift k 4))))
         (+ 1 (reset (* 2 (shift k (k 4)))))
         (+ 1 (reset (* 2 (shift k (k (k 4))))))
         (+ 1 (reset (* 2 (shift k1 (* 3 (shift k2 (k1 (k2 4)))))))))))
(test '(7 5 12 8 18)
      (let* ()
        (define-syntax prompt
          (syntax-rules ()
            [(prompt e1 e2 ...)
             (call-with-continuation-prompt
              (lambda ()
                e1 e2 ...)
              (default-continuation-prompt-tag)
              (lambda (thunk)
                (thunk)))]))
        (define-syntax control
          (syntax-rules ()
            [(control k e1 e2 ...)
             (call-with-composable-continuation
              (lambda (k)
                (abort-current-continuation (default-continuation-prompt-tag)
                  (lambda ()
                    e1 e2 ...))))]))
        (list
         (prompt (+ 2 (control k (k 5))))
         (prompt (+ 2 (control k 5)))
         (prompt (+ 5 (prompt (+ 2 (control k1 (+ 1 (control k2 (k2 6))))))))
         (prompt (+ 5 (prompt (+ 2 (control k1 (+ 1 (control k2 (k1 6))))))))
         (prompt (+ 12 (prompt (+ 5 (prompt (+ 2 (control k1 (control k2 (control k3 (k3 6)))))))))))))

(test #t (continuation? (call/cc values)))
(test #t (continuation? (call-with-composable-continuation values)))

(test 'exception
      (guard (c
	      [(continuation-violation? c) 'exception])
	((call-with-continuation-barrier
	  (lambda ()
	    (call/cc values))))))
(test 'ok
      (call/cc
       (lambda (k)
	 (call-with-continuation-barrier
	  (lambda ()
	    (k 'ok))))))

(test '(#t #t #f)
      (let ([tag (make-continuation-prompt-tag)])
	(call-with-continuation-prompt
	 (lambda ()
	   (list
	    (continuation-prompt-available? tag (call-with-current-continuation values))
	    (continuation-prompt-available? tag (call-with-non-composable-continuation values tag))
	    (continuation-prompt-available? tag (call-with-composable-continuation values tag))))
	 tag)))

(test 7
      (let ([n 0])
	(call/cc
	 (lambda (k)
	   (dynamic-wind
	       values
	       (lambda ()
		 (dynamic-wind
		     values
		     (lambda ()
		       (set! n (+ n 1))
		       (k))
		     (lambda ()
		       (set! n (+ n 2))
		       (k))))
	       (lambda ()
		 (set! n (+ n 4))))))
	n))

(test 'mark
      (with-continuation-mark 'key 'mark
	(call-with-immediate-continuation-mark 'key values)))
(test 'default
      (let ([tag (make-continuation-prompt-tag)])
	(with-continuation-mark 'key 'mark
	  (call-with-continuation-prompt
	   (lambda ()
	     (call-with-immediate-continuation-mark 'key values 'default))
	   tag))))

(test #t (continuation-mark-set? (current-continuation-marks)))

(test '(mark3 mark2)
      (let ([tag (make-continuation-prompt-tag)]
	    [key (make-continuation-mark-key)])
	(with-continuation-mark key 'mark1
	  (with-continuation-mark key 'mark2
	    (call-with-continuation-prompt
	     (lambda ()
	       (with-continuation-mark key 'mark3
		 (continuation-mark-set->list #f key)))
	     tag)))))
(test '(#(mark3 default) #(mark1 mark2))
      (let ([tag (make-continuation-prompt-tag)]
	    [key1 (make-continuation-mark-key)]
	    [key2 (make-continuation-mark-key)])
	(with-continuation-mark key1 'mark1
	  (with-continuation-mark key2 'mark2
	    (call-with-continuation-prompt
	     (lambda ()
	       (with-continuation-mark key1 'mark3
		 (continuation-mark-set->list* #f (list key1 key2) 'default)))
	     tag)))))

(test 'mark2
      (let ([tag (make-continuation-prompt-tag)]
	    [key (make-continuation-mark-key)])
	(with-continuation-mark key 'mark1
	  (call-with-continuation-prompt
	   (lambda ()
	     (with-continuation-mark key 'mark2
	       (continuation-mark-set-first #f key)))
	   tag))))

(test #t (continuation-mark-key? (make-continuation-mark-key)))
(test #f (equal? (make-continuation-mark-key) (make-continuation-mark-key)))

(test '(100 144 20736 144 #t (20736 0) 144)
      (let* ()
	(define p (make-parameter 10 (lambda (x) (* x x))))
	(define ps #f)
	(list (p)
	      (begin (p 12) (p))
	      (parameterize ([p (p)])
		(set! ps (current-parameterization))
		(p))
	      (p)
	      (parameterization? ps)
	      (call-with-parameterization ps
		(lambda ()
		  (let ([x (p)])
		    (p 0)
		    (list x (p)))))
	      (p))))
(test #t
      (with-continuation-mark 'in-tail-context? #t
	(parameterize ([(make-parameter 0) 1])
	  (call-with-immediate-continuation-mark 'in-tail-context? values))))

(test '(#f 1)
      (let ([tag (make-continuation-prompt-tag)]
	    [p (make-parameter 0)])
	(parameterize ([p 1])
	  (call-in-initial-continuation
	   (lambda ()
	     (list (continuation-prompt-available? (call/cc values))
		   (p)))))))
(test 42
      (guard (c
	      [(uncaught-exception-error? c) (uncaught-exception-error-reason c)])
	(call-in-initial-continuation
	 (lambda ()
	   (raise 42)))))

(test #t (promise? (make-promise 1 2)))
(test #t (promise? (delay 3)))
(test #t (promise? (force (make-promise (make-promise 4)))))
(test '(1 2) (call-with-values
		 (lambda () (force (delay
				     (define x 1)
				     (values x 2))))
	       list))
(test 5
      (let* ([p (make-parameter 3)]
	     [q (parameterize
		    ([p 5])
		  (delay (p)))])
	(force q)))

(test 1
      (let* ([x 0]
	     (q (delay
		  (set! x (+ x 1))
		  (raise #t))))
	(guard (c [(uncaught-exception-error? c)])
	  (force q)
	  (set! x (+ x 2)))
	(guard (c [(uncaught-exception-error? c)])
	  (force q)
	  (set! x (+ x 4)))
	x))

(test 43
      (guard (c [(eqv? c 42) c])
	(+ 1
	   (call-with-continuation-prompt
	    (lambda ()
	      (raise 42))))))

(test '(1 2)
      (let ([p (make-parameter 0)])
	(parameterize ([p 1])
	  (let ([y
		 (thread-join!
		  (thread-start!
		   (thread
		    (let ([x (p)])
		      (p 2)
		      x))))])
	    (list y (p))))))

(test 'default (tlref (make-thread-local 'default)))

(test 45 (let ([tl (make-thread-local #f)]) (tlset! tl 45) (tlref tl)))

(test '(1 2)
      (let* ([tl (make-thread-local 1)]
	     [x (thread-join!
		 (thread-start!
		  (thread
		   (tlset! tl 2)
		   (tlref tl))))])
	(list (tlref tl) x)))

;;; Test End

(test-end)

;; Local Variables:
;; mode: scheme
;; End:
