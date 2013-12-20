#|
  This file is a part of pfds-book project.
  Copyright (c) 2013 Stephen A. Goss (steveth45@gmail.com)
|#

(in-package :cl-user)
(defpackage pfds-book-test
  (:use :cl
        :pfds-book
        :cl-test-more))
(in-package :pfds-book-test)

(plan nil)

(defun test-stack (<i>)
  (let* ((stack (empty <i>))
         (stack2 (push-head <i> 'a stack))
         (stack3 (push-head <i> 'b stack2)))
    (is (empty-p <i> stack) T)
    (is (head <i> stack2) 'a)
    (is (empty-p <i> (tail <i> stack2)) T)
    (is (head <i> (tail <i> stack3)) 'a)))

(test-stack <_list>)

(test-stack <custom-stack>)

;; ex 2.1

(is (suffixes '(4 3 2 1))
    '((4 3 2 1) (3 2 1) (2 1) (1) ()))

;; unbalanced-set

(is (_member <unbalanced-set> 10 (tree e-node 10 e-node))
    T)
(is (_member <unbalanced-set> 9 (tree e-node 10 e-node))
    nil)
(is (_member <unbalanced-set> 9 (tree (tree e-node 9 e-node) 10 e-node))
    T)
(is (_member <unbalanced-set> 9 (tree (tree e-node 7 e-node)
                                8
                                (tree e-node 9 e-node)))
    T)
(is (_member <unbalanced-set> 6 (tree (tree e-node 7 e-node)
                                8
                                (tree e-node 9 e-node)))
    nil)

(let* ((empty-set (empty <unbalanced-set>))
       (set1 (insert <unbalanced-set> -5 empty-set))
       (set2 (insert <unbalanced-set> 10 set1)))
  (is (_member <unbalanced-set> 3 set2)
      nil)
  (is (_member <unbalanced-set> 10 set2)
      T))

(defun test-set (<i>)
  (let ((set (empty <i>))
        (test-data '(8 3 20 -99 100 101 2 33 1 -4))
        (first-half '(8 3 20 -99 100))
        (second-half '(101 2 33 1 -4)))
    (loop :for val :in first-half :do (is (_member <i> val set) nil))

    (loop :for val :in first-half :do (setf set (insert <i> val set)))
    (loop :for val :in second-half :do (is (_member <i> val set) nil))
    (loop :for val :in first-half :do (is (_member <i> val set) T))

    (loop :for val :in second-half :do (setf set (insert <i> val set)))
    (loop :for val :in test-data :do (is (_member <i> val set) T))

    (loop :for val :in first-half :do (setf set (_remove <i> val set)))
    (loop :for val :in second-half :do (is (_member <i> val set) T))
    (loop :for val :in first-half :do (is (_member <i> val set) nil))
    (loop :for val :in second-half :do (setf set (_remove <i> val set)))

    (is (empty-p <i> set) T)))

(test-set <unbalanced-set>)

;;  unbalanced-set

(finalize)
