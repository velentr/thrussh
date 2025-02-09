;;; SPDX-FileCopyrightText: 2025 Brian Kubisiak <brian@kubisiak.com>
;;;
;;; SPDX-License-Identifier: GPL-3.0-only

(define-module (thrussh engine)
  #:use-module (thrussh classes)
  #:use-module (thrussh configuration)
  #:use-module (thrussh paths)
  #:use-module (ice-9 sandbox)
  #:use-module (srfi srfi-26)
  #:export (cons-resources
            eval-configuration-in-sandbox
            run))

;;; Commentary:
;;;
;;; Run shell commands in containers.
;;;
;;; Code:

(define (run raw-cmd)
  (command->run (raw->command raw-cmd)))

(define (make-directories dir)
  (define (parent dir)
    (let ((last-/ (string-rindex dir #\/)))
      (if last-/
          (substring dir 0 last-/)
          ".")))
  (unless (file-exists? dir)
    (make-directories (parent dir))
    (mkdir dir)))

(define (cons-resources resources)
  (for-each
   (lambda (resource)
     (let ((path (resource->path resource #:must-exist? #f)))
       (unless (file-exists? path)
         (make-directories path)
         (command->run
          ((resource-class->constructor
            (resource->class resource))
           resource)))))
   resources))

(define (raw->command raw-cmd)
  "Create a structured command object given the RAW-CMD list of strings
requested by the user."
  (define (look-for-cmd remaining-classes)
    (and (not (nil? remaining-classes))
         (or (resource-class->command (car remaining-classes) raw-cmd)
             (look-for-cmd (cdr remaining-classes)))))
  (or (look-for-cmd (%all-resource-classes))
      (error "invalid command:" raw-cmd)))

(define (eval-configuration-in-sandbox config-exp)
  "Evaluate CONFIG-EXP in a sandbox including the resource constructors
from this module."
  (eval-in-sandbox
   config-exp
   #:bindings
   (cons*
    '((thrussh configuration)
      resource*)
    '((thrussh classes)
      %git-resource-class)
    all-pure-bindings)))

;; TODO: This should properly handle escaped spaces and quotes like a shell
(define (parse-command cmd)
  "Split CMD into space-separated words that may be quoted with either single or
double quotes."
  (define (strip-matching char word)
    (and
     (string-prefix? char word)
     (string-suffix? char word)
     (substring word 1 (- (string-length word) 1))))
  (define strip-' (cut strip-matching "'" <>))
  (define strip-'' (cut strip-matching "\"" <>))
  (map
   (lambda (word)
     (or (strip-' word)
         (strip-'' word)
         word))
   (string-split cmd #\space)))
