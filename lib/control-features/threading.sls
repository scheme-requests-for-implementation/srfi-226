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

(library (control-features threading)
  (export %current-dynamic-environment
	  %current-thread %thread? %thread-start! %thread-yield! %thread-terminate!
	  %thread-join!
	  make-%mutex %mutex-lock! %mutex-unlock!
	  make-%condition-variable %condition-variable-broadcast!
	  %run)
  (import (rnrs (6))
	  (control-features primitives)
	  (control-features timer))

  (define *exit-continuation*)
  (define *current-thread*)
  (define *lock* #f)

  (define lock!
    (lambda ()
      (assert (not (locked?)))
      (set! *lock* #t)))

  (define unlock!
    (lambda ()
      (assert (locked?))
      (set! *lock* #f)))

  (define locked?
    (lambda ()
      *lock*))

  (define-record-type %mutex
    (nongenerative) (sealed #t) (opaque #t)
    (fields (mutable locked?))
    (protocol
     (lambda (p)
       (lambda ()
	 (p #f)))))

  (define %mutex-unlock!
    (case-lambda
     [(mtx)
      (assert (%mutex? mtx))
      (%mutex-locked?-set! mtx #f)]
     [(mtx cv)
      (assert (%mutex? mtx))
      (assert (%condition-variable? cv))
      (lock!)
      (%mutex-locked?-set! mtx #f)
      (%condition-variable-waiting-threads-set!
       cv
       (cons (%current-thread)
	     (%condition-variable-waiting-threads cv)))
      (let f ()
	(unlock!)
	(%thread-yield!)
	(lock!)
	(when (memq (%current-thread) (%condition-variable-waiting-threads cv))
	  (f)))
      (unlock!)]))

  (define %mutex-lock!
    (lambda (mtx)
      (assert (%mutex? mtx))
      (let f ()
	(lock!)
	(if (%mutex-locked? mtx)
	    (begin
	      (unlock!)
	      (%thread-yield!)
	      (f))
	    (begin
	      (%mutex-locked?-set! mtx #t)
	      (unlock!))))))

  (define-record-type %condition-variable
    (nongenerative) (sealed #t) (opaque #t)
    (fields (mutable waiting-threads))
    (protocol
     (lambda (p)
       (lambda ()
	 (p '())))))

  (define %condition-variable-broadcast!
    (lambda (cv)
      (assert (%condition-variable? cv))
      (%condition-variable-waiting-threads-set! cv '())))

  (define interrupt-handler
    (lambda ()
      (%set-timer! #t)
      (unless (locked?)
	(%thread-yield!))))

  (define-record-type %thread
    (nongenerative) (opaque #t)
    (fields (mutable continuation)
	    (mutable terminated?))
    (protocol
     (lambda (p)
       (lambda (thunk)
	 (assert (procedure? thunk))
	 (p thunk #f)))))

  (define-record-type %primordial-thread
    (parent %thread)
    (nongenerative) (sealed #t) (opaque #t)
    (protocol
     (lambda (n)
       (lambda ()
	 ((n (lambda () (assert #f))))))))

  (define %current-thread
    (lambda ()
      *current-thread*))

  (define current-threads
    (let ([threads '()])
      (case-lambda
	[() threads]
	[(t) (set! threads t)])))

  (define %current-dynamic-environment
    (let ([environment #f])
      (case-lambda
	[() environment]
	[(env) (set! environment env)])))

  (define %run
    (lambda (thunk)
      (lock!)
      (let-values
	  ([val*
	    (%call-with-current-continuation
	     (lambda (k)
	       (%call-with-interrupt-handler
		interrupt-handler
		(lambda ()
		  (set! *exit-continuation* k)
		  (set! *current-thread* (make-%primordial-thread))
		  (current-threads
		   (list *current-thread*))
		  (unlock!)
		  (thunk)))))])
	(current-threads '())
	(apply values val*))))

  (define %thread-start!
    (lambda (thunk)
      (assert (procedure? thunk))
      (lock!)
      (letrec ([thread
		(make-%thread
		 (lambda ()
		   (unlock!)
		   (thunk)
		   (%thread-terminate! thread)))])
	(current-threads (append (current-threads) (list thread)))
	(unlock!)
	thread)))

  (define %thread-yield!
    (lambda ()
      (lock!)
      (let* ([t* (current-threads)]
	     [ot (car t*)]
	     [t* (append (cdr t*) (list ot))]
	     [nt (car t*)])
	(if (eq? ot nt)
	    (unlock!)
	    (let ([env (%current-dynamic-environment)])
	      (%call-with-current-continuation
	       (lambda (k)
		 (%thread-continuation-set! ot k)
		 (current-threads t*)
		 (abort-to-thread nt)))
	      (%current-dynamic-environment env)
	      (unlock!))))))

  (define %thread-terminate!
    (lambda (thread)
      (lock!)
      (when (%primordial-thread? thread)
	(*exit-continuation*))
      (%thread-terminated?-set! thread #t)
      (let ([t* (remq thread (current-threads))])
       	(current-threads t*)
	(if (eq? (%current-thread) (car t*))
	    (unlock!)
	    (abort-to-thread (car t*))))))

  (define %thread-join!
    (lambda (thread)
      (do ()
	  ((%thread-terminated? thread))
	(%thread-yield!))))

  (define abort-to-thread
    (lambda (thread)
      (set! *current-thread* thread)
      ((%thread-continuation thread)))))

;; Local Variables:
;; mode: scheme
;; End:
