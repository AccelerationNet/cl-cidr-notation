(defpackage #:cl-cidr-notation
  (:use #:cl-user #:cl)
  (:export
   #:cidr-parse-error
   #:ip-string #:cidr-string #:range-string
   #:parse-ip #:parse-cidr
   #:valid-cidr? ))

(defpackage #:cl-cidr-notation-test
  (:use :cl :cl-cidr-notation :lisp-unit))