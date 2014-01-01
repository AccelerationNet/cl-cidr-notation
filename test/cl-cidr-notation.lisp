(in-package :cl-cidr-notation-test)

(define-test basic-tests ()
  (let ((ips (list "0.0.0.0" "0.0.0.0/24" "0.0.0.128/29"
                   "1.12.123.255" "255.255.255.255"
                   "128.128.128.128/29" "0.0.0.0/1"
                   "0.0.0.0/31"
                   )))
    (loop for i in ips
          do (multiple-value-bind (s f) (parse-cidr i)
               (assert-equal i (cidr-string s f))))))

(define-test failures ()
  (let ((ips (list "0.0.0.1/30" "0.0.0.1/31"
                   "0.0.0.1/20" "asdasd" "256.1.1.1"
                   "1.255.1.1.1" ".255.1.1.1"
                   "255.1.1.1." "0.0.0.0." "0.0.0.0/"  )))
    (loop for i in ips
          do (assert-error 'cidr-parse-error (parse-cidr i)))))

(define-test range-string ()
  (assert-equal
   "0.0.1.0-0.0.4.0"
   (range-string 256 1024))
  (assert-equal
   "0.0.1.255-0.0.4.128"
   (range-string 511 1152)))