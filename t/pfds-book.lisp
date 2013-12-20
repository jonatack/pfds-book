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

;; TreeSet

(is (_member <tree-set> 10 (tree e-node 10 e-node))
    T)
(is (_member <tree-set> 9 (tree e-node 10 e-node))
    nil)
(is (_member <tree-set> 9 (tree (tree e-node 9 e-node) 10 e-node))
    T)
(is (_member <tree-set> 9 (tree (tree e-node 7 e-node)
                                8
                                (tree e-node 9 e-node)))
    T)
(is (_member <tree-set> 6 (tree (tree e-node 7 e-node)
                                8
                                (tree e-node 9 e-node)))
    nil)

(let* ((empty-set (empty <tree-set>))
       (set1 (insert <tree-set> -5 empty-set))
       (set2 (insert <tree-set> 10 set1)))
  (is (_member <tree-set> 3 set2)
      nil)
  (is (_member <tree-set> 10 set2)
      T))

;; TreeSet

(finalize)
