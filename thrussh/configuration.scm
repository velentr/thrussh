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
            resource?
            ssh-key*
            ssh-key->authorized-keys-entry
            ssh-key->comment
            ssh-key->id
            ssh-key->key
            ssh-key->key-type
            ssh-key?))

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

;; Specification for an SSH public key.
(define-immutable-record-type <ssh-key>
  (make-ssh-key key-type key comment)
  ssh-key?
  (key-type ssh-key->key-type)
  (key ssh-key->key)
  (comment ssh-key->comment))

(define %supported-ssh-key-types
  '(sk-ecdsa-sha2-nistp256@openssh.com
    ecdsa-sha2-nistp256
    ecdsa-sha2-nistp384
    ecdsa-sha2-nistp521
    sk-ssh-ed25519@openssh.com
    ssh-ed25519
    ssh-rsa))

(define* (ssh-key* #:key key-type key (comment ""))
  (unless (string? key)
    (error "key must be a base64-encoded public key"))
  (unless (member key-type %supported-ssh-key-types)
    (error "unrecognized key type:" key-type))
  (unless (string? comment)
    (error "comment must be a string"))
  (make-ssh-key key-type key comment))

(define (ssh-key->authorized-keys-entry key)
  "Serialize KEY into an entry suitable for ~/.ssh/authorized_keys."
  (string-join (list (symbol->string (ssh-key->key-type key))
                     (ssh-key->key key)
                     (ssh-key->comment key))))

(define (ssh-key->id key)
  "Get a shortened ID that maps 1-1 to KEY."
  (let ((digest (sha256 (string->utf8 (ssh-key->key key)))))
    (base64-encode
     digest
     0 (bytevector-length digest)
     #f #f
     base64url-alphabet)))

;; Reference to a directory stored on-disk that can be bound inside the
;; thrush container.
(define-immutable-record-type <resource>
  (make-resource name class rw ro)
  resource?
  (name resource->name)
  (class resource->class)
  (rw resource->rw)
  (ro resource->ro))

(define* (resource* #:key name type (rw '()) (ro '()))
  "Create a reference to the resource with the given NAME of the given TYPE. The
keys specified in RW have read/write access; the keys specified in RO have
read-only access."
  (define (all pred? lst)
    (or (nil? lst)
        (and (pred? (car lst))
             (all pred? (cdr lst)))))
  (unless (string? name)
    (error "resource name must be a string:" name))
  (unless (resource-class? type)
    (error "type must be a resource class:" type))
  (unless (and (list? rw) (all ssh-key? rw))
    (error "read/write list must contain only SSH keys"))
  (unless (and (list? ro) (all ssh-key? ro))
    (error "read-only list must contain only SSH keys"))
  (make-resource name type rw ro))

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
