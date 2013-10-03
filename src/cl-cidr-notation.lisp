(in-package :cl-cidr-notation)

(define-condition cidr-parse-error ()
  ((input :accessor input :initarg :input :initform nil)
   (idx :accessor idx :initarg :idx :initform nil)
   (hint :accessor hint :initarg :hint :initform nil))
  (:report
   (lambda (c s)
     (format s "~S, idx:~A is not a valid IP/CIDR notation~%~a"
             (input c) (idx c) (hint c)))))

(defun valid-cidr? (int
                    &optional (fixed-bits 32)
                    &aux (mask (- 32 fixed-bits))
                    ;;shift right then left the number of mask bits.
                    (canonical (ash (ash int (- mask)) mask)))
  "Checks whether an int IP with the specified number of fixed bits is
   valid?

   You can write a CIDR address that is not valid but is still parseable
   eg: 0.0.0.1/30 (which should be 0.0.0.0/30 or 0.0.0.4/30)

   If invalid, this returns the correct start address for the notation
    as the second value"
  (values (= int canonical)
          canonical))

(defun cidr-string (start &optional (fixed-bits 32))
  (%cidr-string :start start :fixed-bits fixed-bits))

(defun %bytes-string-length (bytes &aux (rtn 0))
  (declare (type (unsigned-byte 8) rtn))
  (loop for p in bytes
        do (incf rtn
                 (the (unsigned-byte 8)
                      (cond ((< p 10) 1)
                            ((< p 100) 2)
                            (t 3)))))
  rtn)
(declaim (inline byte-string-length))

(defun %ip-string-length (bytes &optional (fixed-bits 32) &aux (rtn 3))
  (declare (type (unsigned-byte 8) rtn))
  (incf rtn (%bytes-string-length bytes))
  (incf rtn (cond ((= 32 fixed-bits) 0)
                  ((< 10 fixed-bits) 3)
                  (T fixed-bits 2)))
  rtn)
(declaim (inline %ip-string-length))


(defun %int-bytes (int)
  (declare (type (unsigned-byte 32) int))
  (list (ldb (byte 8 24) int)
        (ldb (byte 8 16) int)
        (ldb (byte 8 8) int)
        (ldb (byte 8 0) int)))

(declaim (inline byte-string-length))

(defun range-string (s0 s1 &aux idx)
  "returns a string of ip0-ip1
   eg: (range-string 0 1) => 0.0.0.0-0.0.0.1"
  (let* ((b0 (%int-bytes s0))
         (b1 (%int-bytes s1))
         (s (make-string
             (+ 1 (%ip-string-length b0) (%ip-string-length b1))
             :element-type 'base-char
             :initial-element #\.)))
    (setf (values s idx) (%cidr-string :bytes b0 :out-string s))
    (setf (char s idx) #\-)
    (incf idx)
    (setf (values s idx)
          (%cidr-string :bytes b1 :out-string s :start-idx idx))
    s))

(defun %cidr-string (&key
                     start bytes
                     out-string start-idx
                     (fixed-bits 32))
  "Transform an integer to an IP4R ip CIDR range notation.

   Super efficient version from stassats on #lisp
   http://paste.lisp.org/display/139246 "
  (declare
   (type (or null (unsigned-byte 32)) start)
   (type (unsigned-byte 8) fixed-bits)
   (type (or null
             (unsigned-byte 8)) start-idx)
   (type (or null
             simple-base-string) out-string)
   (optimize speed))
  (when (null bytes) (setf bytes (%int-bytes start)) )
  (let* ((string (or out-string
                     (make-string
                      (%ip-string-length bytes fixed-bits)
                      :element-type 'base-char :initial-element #\.)))
         (index (or start-idx 0)))
    (declare (optimize (safety 0)))
    (labels ((put-char (char)
               (setf (char string index) char)
               (incf index))
             (put-digit-char (char)
               (put-char (code-char (+ 48 char))))
             (put-number (number &aux rem0 rem1 rem2)
               (declare (type (unsigned-byte 8) number))
               (setf (values number rem0) (truncate number 10))
               (when (plusp number)
                 (setf (values number rem1) (truncate number 10)))
               (when (plusp number)
                 (setf (values number rem2) (truncate number 10)))
               (when rem2 (put-digit-char rem2))
               (when rem1 (put-digit-char rem1))
               (when rem0 (put-digit-char rem0))))
      (declare (inline put-number put-char put-digit-char))
      (put-number (pop bytes))(incf index)
      (put-number (pop bytes))(incf index)
      (put-number (pop bytes))(incf index)
      (put-number (pop bytes))
      (when (< fixed-bits 32)
        (put-char #\/)
        (put-number fixed-bits))
      (values string index))))

(defun ip-string (int-address)
  "An alias for cidr-string"
  (cidr-string int-address))

(defun parse-cidr (string &key (validate? t)
                   &aux
                   (i (length string))
                   (first-idx (- i 1))
                   (pow 0) (sum 0) (part 0) (tri 0)
                   (c #\0) (fixed 32))
  "Speedy ip-address / CIDR parser

   summary:
     we are going backwards down the string reading each number and summing it

     when we finish a triplet we shift that into the triplet number's byte and
     sum that into the result"
  (declare (type character c)
           (type (unsigned-byte 8) i)
           (type simple-string string))
  (flet ((add-part ()
           (when (> part 255)
             (error 'cidr-parse-error
                    :input string
                    :idx i
                    :hint "Contained a triplet larger than 255"))
           (when (> tri 3)
             (error 'cidr-parse-error
                    :input string
                    :idx i
                    :hint "Contained too many triplets"))
           (incf sum (ash part (* tri 8)))))
    (declare (inline add-part))
    (loop
      while (plusp i)
      do (progn
           (decf i)
           (setf c (char string i))
           (cond
             ((char<= #\0 c #\9)
              (incf part (* (- (char-code c) 48)
                            (expt 10 pow)))
              (incf pow))
             ((char= #\. c)
              (add-part)
              (incf tri)
              (setf pow 0 part 0))
             ((char= #\/ c)
              (when (= first-idx i)
                (error 'cidr-parse-error
                       :input string
                       :idx i
                       :hint "IP Cannot end in /"))
              (setf fixed part
                    part 0
                    pow 0))
             (t (error 'cidr-parse-error
                       :input string
                       :idx i
                       :hint "Contained an invalid character")))))
    (add-part))
  (unless (<= 0 fixed 32)
    (error 'cidr-parse-error
           :input string
           :idx i
           :hint "has an invalid prefix-size/mask/fixed-bits"))
  (when (and (/= fixed 32) ;; single IPs cannot be invalid
             validate? (not (valid-cidr? sum fixed)))
    (error 'cidr-parse-error
           :input string
           :idx i
           :hint "Parsed but was an invalid CIDR address either the range started at the wrong place"))
  (values sum fixed))

(defun parse-ip (input)
  "an alias for parse-CIDR but ensures its a single IP"
  (multiple-value-bind (s f)
      (parse-cidr input)
    (unless (= f 32)
      (error 'cidr-parse-error
             :input input
             :idx 0
             :hint "Parsed but was not a valid single IP (call parse-cidr?)"))
    s))