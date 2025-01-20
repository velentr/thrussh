;;; SPDX-FileCopyrightText: 2025 Brian Kubisiak <brian@kubisiak.com>
;;;
;;; SPDX-License-Identifier: GPL-3.0-only

(define-module (thrussh classes)
  #:use-module (thrussh configuration)
  #:use-module (ice-9 match)
  #:export (%all-resource-classes
            %git-resource-class))

;;; Commentary:
;;;
;;; Define resource classes.
;;;
;;; Code:

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

(define %all-resource-classes
  ;; TODO: load these dynamically
  (list %git-resource-class))
