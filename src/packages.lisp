(defpackage #:cl-cidr-notation
  (:use #:cl-user #:cl)
  (:export
   #:cidr-parse-error
   #:cidr-string
   #:ip-string
   #:ipv6-string
   #:parse-cidr
   #:parse-ipv6-cidr
   #:parse-ip
   #:parse-ipv6
   #:range-string
   #:valid-cidr?))
