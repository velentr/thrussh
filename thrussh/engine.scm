;;; SPDX-FileCopyrightText: 2025 Brian Kubisiak <brian@kubisiak.com>
;;;
;;; SPDX-License-Identifier: GPL-3.0-only

(define-module (thrussh engine)
  #:use-module (thrussh paths)
  #:use-module (ice-9 match)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-9)
  #:use-module (gcrypt base64)
  #:use-module (gcrypt hash)
  #:export (cons-resources
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
  (or (file-exists? dir)
      (begin
        (make-directories (parent dir))
        (mkdir dir))))

(define-record-type <command>
  (make-command operation args packages exposures shares)
  command?
  (operation command->operation)
  (args command->args)
  (packages command->packages)
  (exposures command->exposures)
  (shares command->shares))

(define* (command*
          #:key operation args (packages '()) (exposures '()) (shares '()))
  "Create a new command for running OPERATION with the given ARGS, to be run in
a container with guix PACKAGES. EXPOSURES are mounted read-only inside the
container; SHARES are mounted read-write."
  (make-command operation args packages exposures shares))

(define-record-type <resource-class>
  (make-resource-class name constructor handler)
  resource-class?
  (name resource-class->name)
  (constructor resource-class->constructor)
  (handler resource-class->handler))

(define-record-type <resource>
  (make-resource name class)
  resource?
  (name resource->name)
  (class resource->class))

(define* (resource* #:key name type)
  "Create a reference to the resource with the given NAME of the given TYPE."
  (make-resource name type))

(define (resource->type resource)
  "Get the type of RESOURCE, as a string."
  (resource-class->name (resource->class resource)))

(define (resource->global-name resource)
  "Get the globally-unique name for RESOURCE."
  (string-append (resource->type resource) "/" (resource->name resource)))

(define* (resource->path resource #:key (must-exist? #t))
  "Find where RESOURCE is stored on disk, raising an error if it does not
exist."
  (let* ((name (resource->global-name resource))
         (digest (sha256 (string->utf8 name)))
         (relpath
          ;; 'guix shell --expose' and '--share' use '=' to separate the path
          ;; inside the container and outside the container, so it can't be used
          ;; as part of the base64 padding. Fortunately, all our base64 strings
          ;; are the same length, so we can just trim the padding before using
          ;; it as a path.
          (string-trim-right
           (base64-encode
            digest
            0 (bytevector-length digest)
            #f #f
            base64url-alphabet)
           #\=))
         (maybe-path (xdg-data-home "thrussh" "res" relpath)))
    (when (and must-exist? (not (file-exists? maybe-path)))
      (error "resource does not exist:" name))
    maybe-path))

(define* (resource-class* #:key name constructor handler)
  (make-resource-class name constructor handler))

(define (resource-class->command? class raw-cmd)
  "Make a CLASS <command> from RAW-CMD, if it is the correct class. Otherwise,
returns #F."
  ((resource-class->handler class) raw-cmd))

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

(define (command->run cmd)
  (define (make-spec arg)
    (lambda (resource)
      (let ((inside (~ (resource->name resource)))
            (outside (resource->path resource)))
        (string-append arg "=" outside "=" inside))))
  (let ((exposures (map (make-spec "--expose") (command->exposures cmd)))
        (shares (map (make-spec "--share") (command->shares cmd))))
    (apply
     system*
     (append
      '("guix" "shell" "--container" "--pure" "--no-cwd" "--user=thrussh")
      exposures
      shares
      (command->packages cmd)
      '("--")
      (command->args cmd)))))

(define %git-resource-class
  (resource-class*
   #:name "git"
   #:constructor
   (lambda (resource)
     (command*
      #:operation 'cons
      #:args (list "git" "init" "--bare" (resource->name resource))
      #:packages '("git")
      #:shares (list resource)))
   #:handler
   (lambda (raw-cmd)
     (match raw-cmd
       (("git-receive-pack" repo-name)
        (command*
         #:operation 'write
         #:args raw-cmd
         #:packages '("git")
         #:shares (list (resource*
                         #:name repo-name
                         #:type %git-resource-class))))
       (("git-upload-pack" repo-name)
        (command*
         #:operation 'read
         #:args raw-cmd
         #:packages '("git")
         #:exposures (list (resource*
                            #:name repo-name
                            #:type %git-resource-class))))
       (_ #f)))))


(define all-resource-classes
  ;; TODO: load these dynamically
  (list %git-resource-class))

(define (raw->command raw-cmd)
  "Create a structured command object given the RAW-CMD list of strings
requested by the user."
  (define (look-for-cmd remaining-classes)
    (and (not (nil? remaining-classes))
         (or (resource-class->command? (car remaining-classes) raw-cmd)
             (look-for-cmd (cdr remaining-classes)))))
  (or (look-for-cmd (all-resource-classes))
      (error "invalid command:" raw-cmd)))
