#|
  This file is a part of pfds-book project.
  Copyright (c) 2013 Stephen A. Goss (steveth45@gmail.com)
|#

(in-package :cl-user)
(defpackage pfds-book-test-asd
  (:use :cl :asdf))
(in-package :pfds-book-test-asd)

(defsystem pfds-book-test
  :author "Stephen A. Goss"
  :license "Modified BSD"
  :depends-on (:pfds-book
               :cl-test-more)
  :components ((:module "t"
                :components
                ((:file "pfds-book"))))
  :perform (load-op :after (op c) (asdf:clear-system c)))
