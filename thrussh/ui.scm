;;; SPDX-FileCopyrightText: 2025 Brian Kubisiak <brian@kubisiak.com>
;;;
;;; SPDX-License-Identifier: GPL-3.0-only

(define-module (thrussh ui)
  #:use-module (thrussh engine)
  #:use-module (thrussh paths)
  #:use-module (ice-9 exceptions)
  #:use-module (ice-9 match)
  #:export (main))

(define (load-configuration)
  "Load the configuration file from ~/.config/thrussh/config.scm and evaluate it
in a sandbox."
  (let ((config-sexp
         (call-with-input-file (xdg-config-home "thrussh" "config.scm")
           (lambda (conf)
             (define (accumulate-sexps so-far)
               (let ((maybe-sexp (read conf)))
                 (if (eof-object? maybe-sexp)
                     (reverse so-far)
                     (accumulate-sexps (cons maybe-sexp so-far)))))
             (accumulate-sexps '())))))
    (eval-configuration-in-sandbox
     (cons 'begin config-sexp))))

(define (main)
  (with-exception-handler
   (lambda (exn)
     (apply
      format
      (cons*
       (current-error-port) (exception-message exn) (exception-irritants exn)))
     (newline (current-error-port))
     (exit 1))
   (lambda ()
     (let ((args (cdr (command-line))))
       (match args
         (("cons")
          (cons-resources (load-configuration)))
         (()
          (begin
            (display "usage: thrussh cons")
            (newline)))
         (_
          (error "unrecognized command:" (car args))))))
   #:unwind? #t))
