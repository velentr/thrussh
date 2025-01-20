;;; SPDX-FileCopyrightText: 2025 Brian Kubisiak <brian@kubisiak.com>
;;;
;;; SPDX-License-Identifier: GPL-3.0-only

(define-module (thrussh configuration)
  #:use-module (thrussh paths)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (gcrypt base64)
  #:use-module (gcrypt hash)
  #:export (command*
            command->args
            command->exposures
            command->operation
            command->packages
            command->run
            command->shares
            command?
            resource-class*
            resource-class->command
            resource-class->constructor
            resource-class->handler
            resource-class->name
            resource-class?
            resource*
            resource->class
            resource->global-name
            resource->name
            resource->path
            resource->type
            resource?))

;;; Commentary:
;;;
;;; Functions and records for configuring thrussh.
;;;
;;; Code:

;; Define methods for manipulating resources.
(define-immutable-record-type <resource-class>
  (make-resource-class name constructor handler)
  resource-class?
  (name resource-class->name)
  (constructor resource-class->constructor)
  (handler resource-class->handler))

(define (resource-class->command class raw-cmd)
  "Make a CLASS <command> from RAW-CMD, if it is the correct class. Otherwise,
returns #F."
  ((resource-class->handler class) raw-cmd))

(define* (resource-class* #:key name constructor handler)
  "Create a resource class NAME that builds resources with CONSTRUCTOR and
matches ssh commands with HANDLER."
  (unless (string? name)
    (error "resource class name must be a string:" name))
  (make-resource-class name constructor handler))

;; Reference to a directory stored on-disk that can be bound inside the
;; thrush container.
(define-immutable-record-type <resource>
  (make-resource name class)
  resource?
  (name resource->name)
  (class resource->class))

(define* (resource* #:key name type)
  "Create a reference to the resource with the given NAME of the given TYPE."
  (unless (string? name)
    (error "resource name must be a string:" name))
  (unless (resource-class? type)
    (error "type must be a resource class:" type))
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

(define-immutable-record-type <command>
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
