(in-package :cl-cidr-notation)

(define-condition cidr-parse-error (error)
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

(defmacro %bytes-string-length (a b c d rtn-init
                                  &aux (rtn (gensym "RTN")))
  `(let ((,rtn ,rtn-init))
    ,@(loop for v in (list a b c d)
            collect `(incf ,rtn
                           (the (unsigned-byte 8)
                            (cond ((< ,v 10) 1)
                                  ((< ,v 100) 2)
                                  (t 3)))))
    ,rtn))

(defmacro %ip-string-length (a b c d &optional (fixed-bits 32))
  `(%bytes-string-length
    ,a ,b ,c ,d
    (+ 3 (the (unsigned-byte 8)
          (cond ((= 32 ,fixed-bits) 0)
                ((< 10 ,fixed-bits) 3)
                (T ,fixed-bits 2))))))



(defmacro %int-bytes ((name &optional (int name)) &body body)
  `(let ((,(symbol-munger:combine-symbols (list name 0))
          (ldb (byte 8 24) ,int))
         (,(symbol-munger:combine-symbols (list name 1))
          (ldb (byte 8 16) ,int))
         (,(symbol-munger:combine-symbols (list name 2))
          (ldb (byte 8 8) ,int))
         (,(symbol-munger:combine-symbols (list name 3))
          (ldb (byte 8 0) ,int)))
    ,@body))


(defun range-string (s0 s1 &aux idx)
  "returns a string of ip0-ip1
   eg: (range-string 0 1) => 0.0.0.0-0.0.0.1"
  (%int-bytes (s0)
    (%int-bytes (s1)
      (let* ((s (make-string
                 (+ 1 (%ip-string-length s0-0 s0-1 s0-2 s0-3)
                    (%ip-string-length s1-0 s1-1 s1-2 s1-3))
                 :element-type 'base-char
                 :initial-element #\.)))
        (setf (values s idx) (%cidr-string s0-0 s0-1 s0-2 s0-3 :out-string s))
        (setf (char s idx) #\-)
        (incf idx)
        (setf (values s idx)
              (%cidr-string s1-0 s1-1 s1-2 s1-3 :out-string s :start-idx idx))
        s))))

(defun %cidr-string (a b c d &key
                     out-string start-idx
                     (fixed-bits 32))
  "Transform an integer to an IP4R ip CIDR range notation.

   Super efficient version from stassats on #lisp
   http://paste.lisp.org/display/139246 "
  (declare
   (type (unsigned-byte 8) fixed-bits a b c d)
   (type (or null
             (unsigned-byte 8)) start-idx)
   (type (or null
             simple-base-string) out-string)
   (optimize speed))
  (let* ((string (or out-string
                     (make-string
                      (%ip-string-length a b c d fixed-bits)
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
      (put-number a)(incf index)
      (put-number b)(incf index)
      (put-number c)(incf index)
      (put-number d)
      (when (< fixed-bits 32)
        (put-char #\/)
        (put-number fixed-bits))
      (values string index))))

(defun %cidr-string-old (start
                    &key
                    out-string start-idx
                    (fixed-bits 32))
  "Transform an integer to an IP4R ip CIDR range notation.

   Super efficient version from stassats on #lisp
   http://paste.lisp.org/display/139246 "
  (declare (type (unsigned-byte 32) start)
           (type (unsigned-byte 8) fixed-bits)
           (type (or null
                     (unsigned-byte 8)) start-idx)
           (type (or null
                     simple-base-string) out-string)
           (optimize speed))
  (flet ((ip-length (part)
           (cond ((< part 10) 1)
                 ((< part 100) 2)
                 (t 3))))
    (declare (inline ip-length))
    (let* ((a (ldb (byte 8 24) start))
           (b (ldb (byte 8 16) start))
           (c (ldb (byte 8 8) start))
           (d (ldb (byte 8 0) start))
           (fixed-len (cond ((= 32 fixed-bits) 0)
                            ((< 10 fixed-bits) 3)
                            (T fixed-bits 2)))
           (string (or
                    out-string
                    (make-string
                     (+ 3
                        (ip-length a)
                        (ip-length b)
                        (ip-length c)
                        (ip-length d)
                        fixed-len)
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
        (put-number a)(incf index)
        (put-number b)(incf index)
        (put-number c)(incf index)
        (put-number d)
        (when (< fixed-bits 32)
          (put-char #\/)
          (put-number fixed-bits))
        (values string index)))))

(defun cidr-string (start &optional (fixed-bits 32))
  (declare (type (unsigned-byte 32) start)
           (type (unsigned-byte 8) fixed-bits))
  (%int-bytes (i start)
    (%cidr-string i-0 i-1 i-2 i-3 :fixed-bits fixed-bits)))

(defun ip-string (int-address)
  "An alias for cidr-string"
  (declare (type (unsigned-byte 32) int-address))
  (cidr-string int-address))

(defun parse-cidr (cidr-string &key (validate? t)
                   &aux
                   (i (length cidr-string))
                   (first-idx (- i 1))
                   (pow 0)
                   (sum 0)
                   (part 0)
                   (tri 0)
                   (c #\0)
                   (fixed 32))
  "Speedy ip-address / CIDR parser
   Summary:
     we are going backwards down the string, reading each number and summing it.
     When we finish a triplet, we shift that into the triplet number's byte and
     sum that into the result"
  (declare (type character c)
           (type (unsigned-byte 8) i)
           (type (or string simple-string) cidr-string))
  (flet ((add-part ()
           (when (> part 255)
             (error 'cidr-parse-error
                    :input cidr-string
                    :idx i
                    :hint "Contained a triplet larger than 255"))
           (when (> tri 3)
             (error 'cidr-parse-error
                    :input cidr-string
                    :idx i
                    :hint "Contained too many triplets"))
           (incf sum (ash part (* tri 8)))))
    (declare (inline add-part))
    (loop
      while (plusp i)
      do (progn
           (decf i)
           (setf c (char cidr-string i))
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
                       :input cidr-string
                       :idx i
                       :hint "IP Cannot end in /"))
              (setf fixed part
                    part 0
                    pow 0))
             (t (error 'cidr-parse-error
                       :input cidr-string
                       :idx i
                       :hint "Contained an invalid character")))))
    (add-part))
  (unless (<= 0 fixed 32)
    (error 'cidr-parse-error
           :input cidr-string
           :idx i
           :hint "has an invalid prefix-size/mask/fixed-bits"))
  (when (and (/= fixed 32) ;; single IPs cannot be invalid
             validate?
             (not (valid-cidr? sum fixed)))
    (error 'cidr-parse-error
           :input cidr-string
           :idx i
           :hint "Parsed but was an invalid CIDR address. The range may have started at the wrong place"))
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
