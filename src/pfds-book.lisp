#|
  This file is a part of pfds-book project.
  Copyright (c) 2013 Stephen A. Goss (steveth45@gmail.com)
|#

(in-package :cl-user)
(defpackage pfds-book
  (:use :cl)
  (:export #:empty
           #:empty-p
           #:push-head
           #:head
           #:tail
           #:_nil
           #:_cons
           #:<container>
           #:<stack>
           #:<_list>
           #:<custom-stack>
           #:<set>
           #:<unbalanced-set>
           #:e-node
           #:tree
           #:insert
           #:_member
           )
  )
(in-package :pfds-book)

;; Interface class for all containers

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass <container> () ()))

;; tuple

(defmacro deftuple (name fields)
  `(progn
     (defparameter ,name ',name)
     (defun ,name (,@fields)
       (vector ',name ,@fields))))

(defun tuplep (tuple)
  (simple-vector-p tuple))

(defun tuple-tag (tuple)
  (svref tuple 0))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun smatch-process-tuplecase (tuplecase inputvarname)
    (let* ((matchspec (first tuplecase))
           (tag (first matchspec))
           (binding-names (rest matchspec))
           (pos 0)
           (bindings (mapcar (lambda (name)
                               `(,name (svref ,inputvarname ,(incf pos))))
                             binding-names)))
      `(,tag
        (let (,@bindings)
          ,(second tuplecase))))))

(defmacro smatch (input &rest cases)
  (let ((symbolcases '())
        (tuplecases '()))
    (mapcan
     (lambda (case)
       (let ((match (first case)))
         (etypecase match
           (symbol (push case symbolcases))
           (cons (push case tuplecases)))))
     cases)
    (setf symbolcases (reverse symbolcases))
    (setf tuplecases (reverse tuplecases))
    (append
     (let ((ginput (gensym)))
       `(let ((,ginput ,input))
          (etypecase ,ginput
            (symbol
             (ecase ,ginput ,@symbolcases))
            (simple-vector
             (ecase (tuple-tag ,ginput)
               ,@(mapcar (lambda (tuplecase)
                           (smatch-process-tuplecase tuplecase ginput))
                         tuplecases)))))))))

;; Chapter 2: Persistence

;; Stack

;; Stack Interface

(defgeneric empty (<container>))
(defgeneric empty-p (<container> seq)) ;; instead of isEmpty
(defgeneric push-head (<container> elem seq)) ;; instead of cons
(defgeneric head (<container> seq))
(defgeneric tail (<container> seq))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass <stack> (<container>) ())
  (defparameter <stack> (make-instance '<stack>)))

;; _List

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass <_list> (<stack>) ())
  (defparameter <_list> (make-instance '<_list>)))

(defmethod empty ((<i> <_list>))
  nil)
(defmethod empty-p ((<i> <_list>) list)
  (null list))
(defmethod push-head ((<i> <_list>) elem list)
  (cons elem list))
(defmethod head ((<i> <_list>) list)
  (first list))
(defmethod tail ((<i> <_list>) list)
  (rest list))

;; CustomStack

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass <custom-stack> (<stack>) ())
  (defparameter <custom-stack> (make-instance '<custom-stack>)))
(defparameter _nil '_nil)
(deftuple _cons (head tail))

(defmethod empty ((<i> <custom-stack>))
  _nil)
(defmethod empty-p ((<i> <custom-stack>) stack)
  (eq _nil stack))
(defmethod push-head ((<i> <custom-stack>) elem stack)
  (_cons elem stack))
(defmethod head ((<i> <custom-stack>) stack)
  (smatch stack
          (_nil (error "empty"))
          ((_cons head tail) head)))
(defmethod tail ((<i> <custom-stack>) stack)
  (smatch stack
          (_nil (error "empty"))
          ((_cons head tail) tail)))

;; Tree datatype

(defparameter e-node 'e-node)
(deftuple tree (left-node elem right-node))

;; Set interface

;; empty (already defined)
;; empty-p (already defined)
(defgeneric insert (<container> elem set))
(defgeneric _member (<container> elem set))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass <set> (<container>) ())
  (defparameter <set> (make-instance '<set>)))

;; unbalanced-set

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass <unbalanced-set> (<set>)
    ((lt :initarg :lt :initform #'< :reader lt)))
  (defparameter <unbalanced-set> (make-instance '<unbalanced-set>)))

(defmethod empty ((<i> <unbalanced-set>))
  e-node)

(defmethod empty-p ((<i> <unbalanced-set>) set)
  (eq set e-node))

(defmethod insert ((<i> <unbalanced-set>) x set)
  (smatch set
          (e-node (tree e-node x e-node))
          ((tree left y right)
           (let ((lt (lt <i>)))
             (if (funcall lt x y)
                 (tree (insert <i> x left) y right)
                 (if (funcall lt y x)
                     (tree left y (insert <i> x right))
                     set))))))

(defmethod _member ((<i> <unbalanced-set>) x set)
  (smatch set
          (e-node nil)
          ((tree left y right)
           (let ((lt (lt <i>)))
             (if (funcall lt x y)
                 (_member <i> x left)
                 (if (funcall lt y x)
                     (_member <i> x right)
                     T))))))
