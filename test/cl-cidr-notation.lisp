(in-package :cl-cidr-notation-test)


;;; IPv4

(define-test basic-tests ()
  (let ((ips (list "0.0.0.0"
                   "0.0.0.0/24"
                   "0.0.0.0/1"
                   "0.0.0.0/31"
                   "0.0.0.128/29"
                   "1.12.123.255"
                   "128.128.128.128/29"
                   "255.255.255.255")))
    (loop for i in ips
          do (multiple-value-bind (s f) (parse-cidr i)
               (assert-equal i (cidr-string s f))))))

(define-test failures ()
  (let ((ips (list "0.0.0.0."
                   "0.0.0.0/"
                   "0.0.0.1/30"
                   "0.0.0.1/31"
                   "0.0.0.1/20"
                   "asdasd"
                   "256.1.1.1"
                   "1.255.1.1.1"
                   ".255.1.1.1"
                   "255.1.1.1.")))
    (loop for i in ips
          do (assert-error 'cidr-parse-error (parse-cidr i)))))

(define-test range-string ()
  (assert-equal
   "0.0.1.0-0.0.4.0"
   (range-string 256 1024))
  (assert-equal
   "0.0.1.255-0.0.4.128"
   (range-string 511 1152)))


;;; IPv6

(define-test basic-ipv6 ()
  ;; Loopback address
  (assert-equal 1 (parse-ipv6 "::1"))
  ;; Network address
  (assert-equal "cafe:beef:0000:0000:0000:0000:0000:0000"
                (ipv6-string
                  (parse-ipv6 "cafe:beef::")))
  ;; Regular address
  (assert-equal "cafe:beef:0000:0000:0000:0000:0000:0001"
                (ipv6-string
                  (parse-ipv6 "cafe:beef::1")))
  ;; Subnet
  (multiple-value-bind (int prefix)
    (parse-ipv6-cidr "cafe:beef::/64")
    (assert-equal "cafe:beef:0000:0000:0000:0000:0000:0000"
                  (ipv6-string int))
    (assert-equal 64 prefix))
  ;; Interface address
  (multiple-value-bind (int prefix)
    (parse-ipv6-cidr "cafe:beef::1")
    (assert-equal "cafe:beef:0000:0000:0000:0000:0000:0001"
                  (ipv6-string int))
    (assert-equal 128 prefix)))
