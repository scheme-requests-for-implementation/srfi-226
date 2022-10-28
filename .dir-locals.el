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

((scheme-mode
  . ((eval
      . (progn
	  (put '%case-lambda-box 'scheme-indent-function 1)
          (put 'guard 'scheme-indent-function 1)
          (put 'with-continuation-mark 'scheme-indent-function 2)
          (put 'with-continuation-marks 'scheme-indent-function 1)
          (put 'call-in-continuation 'scheme-indent-function 1)
          (put 'call-with-immediate-continuation-mark 'scheme-indent-function 1)
          (put 'call-with-parameterization 'scheme-indent-function 1)
	  (put 'abort-current-continuation 'scheme-indent-function 1)
	  (put 'meta-continuation-case 'scheme-indent-function 1)
	  (put 'unwind-protect 'scheme-indent-function 1)
	  (put 'rec 'scheme-indent-function 1)
	  (put 'with-syntax 'scheme-indent-function 1)
          (font-lock-add-keywords
           nil
           '(("(\\(define/who\\|define-record-type\\|define-syntax/who\\|define-condition-type\\)\\>[ \t]*(*\\(\\sw+\\)?"
              (1 font-lock-keyword-face)
              (2 font-lock-function-name-face nil t))
	     ("(\\(%case-lambda-box\\)\\>" 1 font-lock-keyword-face)
	     ("(\\(unwind-protect\\)\\>" 1 font-lock-keyword-face)
	     ("(\\(call-in-continuation\\)\\>" 1 font-lock-keyword-face)
	     ("(\\(call-with-parameterization\\)\\>" 1 font-lock-keyword-face)
	     ("(\\(with-continuation-mark\\)\\>" 1 font-lock-keyword-face)
	     ("(\\(with-continuation-marks\\)\\>" 1 font-lock-keyword-face)
             ("(\\(call-with-immediate-continuation-mark\\)\\>" 1 font-lock-keyword-face)
	     ("(\\(with-syntax\\)\\>" 1 font-lock-keyword-face)
	     ("(\\(rec\\)\\>" 1 font-lock-keyword-face)
	     )))))))
