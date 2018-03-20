(in-package :cl-cidr-notation)

;;;; IPv6 string <--> integer functions

(defun hex-char-p (cha)
  "Check whether a character is a valid hexadecimal one.
   Return a boolean"
  (when
    (or (digit-char-p cha)
        (member cha (list #\a #\b #\c #\d #\e #\f #\A #\B #\C #\D #\E #\F)))
    t))

;;; FIXME Replace tokenise-ipv6-basic with this, once it's working
#+(or)
(defun ipv6-string-parser (ipv6-string
                            &key (index 0) ; pointer into the array
                            chunks ; Accumulator for the parsed chunks
                            str    ; Accumulator for the string being built
                            colonp ; Boolean indicating whether the last char was a colon
                            (num-before-dc 0) ; The number of chunks parsed before ::
                            dc-found-p ; Boolean indicating whether we've already found a ::
                            )
  "Parses an IPv6 string into chunks, plus a count of the chunks parsed before encountering ::
   Return a two-element list:
   - the accumulated chunks
   - the number of chunks represented by ::"
  (cond
    ;; End of the string.
    ;; Finish processing and pass it back
    ;; FIXME: write the actual code
    ((= index (length ipv6-string))
     (error "Actual code goes here"))
    ;; String starts with a colon
    ((and (equal index 0)
          (equal (aref ipv6-string index) #\:))
     (ipv6-string-parser ipv6-string
                         :index 1
                         :colonp t))
    ;; Found a double-colon
    ((and colonp
          (equal (aref ipv6-string index) #\:))
     (ipv6-string-parser ipv6-string
                         :index (+ index 1)
                         :colonp t
                         :chunks chunks
                         :dc-found-p t))
    ((hex-char-p (aref ipv6-string index))
     (ipv6-string-parser ipv6-string
                         :index (+ index 1)))
    ;; Catch-all fallback
    (t
     (error "Unhandled state"))))

(defun ipv6-chunks-to-int (chunks ; Remaining chunks to parse
                            acc   ; Accumulator for the result
                            zero-length   ; Number of consecutive zero-chunks
                            chunks-done)  ; Number of chunks processed so far
  "Interpret the parsed chunks of an IPv6 string, and return an integer."
  ;; Have we already hit the end of the list?
  (if chunks
      ;; Not at the end of the list yet
      ;; First check: is this where the :: appears?
      (if (null (car chunks))
          ;; This is where the :: appears.
          ;; Reduce zero-length to 0, to stop left-shifting things to its right
          (progn
            (ipv6-chunks-to-int (cdr chunks) acc 0 (+ chunks-done zero-length)))
          ;; Not the :: section. Parse as a hex section.
          (progn
            (ipv6-chunks-to-int
              (cdr chunks)
              ;; This is where the fun stuff happens:
              ;; - read the string as a number in hexadecimal notation
              ;; - left-shift it by a suitable amount
              ;; - add that result to the accumulator
              (logior acc
                      (ash (parse-integer (car chunks) :radix 16)
                           (* 16 (- 7 chunks-done))))
              ;; Just pass this on through until it's needed
              zero-length
              ;; Increment the number of chunks processed
              (+ chunks-done 1))))
      ;; If we've hit the end, return the accumulator
      acc))

(defun tokenise-ipv6-basic (ipv6-string)
  "Naive approach to parsing an IPv6 string.
   Return a list of the chunks making up the string, with :: represented by a NIL element."
  (let ((string-parts (cl-ppcre:split ":" ipv6-string)))
    ;; Strings beginning with :: are split into a list with a spurious leading ""
    ;; so remove it.
    (substitute nil
                ""
                (if (equal (char ipv6-string 0) #\:)
                    (cdr string-parts)
                    string-parts)
                :test #'equal)))

(defun parse-ipv6 (ipv6-string)
  "Somewhat basic approach to parsing an IPv6 string."
  (let ((chunks (tokenise-ipv6-basic ipv6-string)))
    (ipv6-chunks-to-int
      chunks
      0
      ;; Calculate the number of zero-chunks represented by ::, if anything
      (- 8 (length (remove-if #'null chunks)))
      0)))

(defun parse-ipv6-cidr (cidrstr)
  "Transform an IPv6 string such as cafe:beef::/64 into a pair of integers
   representing the network address and the prefix-length.
   If a bare address is given, assert the prefix-length as 128."
  (let* ((slash (position #\/ cidrstr))
         (addr (subseq cidrstr 0 slash))
         (prefixlength
           (if slash
               (parse-integer (subseq cidrstr (+ slash 1)) :junk-allowed nil)
               128)))
    (values (cl-cidr-notation:parse-ipv6 addr)
            prefixlength)))

(defun ipv6-string (int)
  "Simple approach to rendering an integer in canonical IPv6 string format.
   Does not strip leading zeros, or replace repeated sections of zeros with ::."
  (string-downcase ;; Because :case :downcase doesn't work in SBCL
    (with-output-to-string (outstr)
      (write (ldb (byte 4 124) int) :base 16 :stream outstr)
      (write (ldb (byte 4 120) int) :base 16 :stream outstr)
      (write (ldb (byte 4 116) int) :base 16 :stream outstr)
      (write (ldb (byte 4 112) int) :base 16 :stream outstr)
      (write ":" :stream outstr :escape nil)
      (write (ldb (byte 4 108) int) :base 16 :stream outstr)
      (write (ldb (byte 4 104) int) :base 16 :stream outstr)
      (write (ldb (byte 4 100) int) :base 16 :stream outstr)
      (write (ldb (byte 4 96) int) :base 16 :stream outstr)
      (write ":" :stream outstr :escape nil)
      (write (ldb (byte 4 92) int) :base 16 :stream outstr)
      (write (ldb (byte 4 88) int) :base 16 :stream outstr)
      (write (ldb (byte 4 84) int) :base 16 :stream outstr)
      (write (ldb (byte 4 80) int) :base 16 :stream outstr)
      (write ":" :stream outstr :escape nil)
      (write (ldb (byte 4 76) int) :base 16 :stream outstr)
      (write (ldb (byte 4 72) int) :base 16 :stream outstr)
      (write (ldb (byte 4 68) int) :base 16 :stream outstr)
      (write (ldb (byte 4 64) int) :base 16 :stream outstr)
      (write ":" :stream outstr :escape nil)
      (write (ldb (byte 4 60) int) :base 16 :stream outstr)
      (write (ldb (byte 4 56) int) :base 16 :stream outstr)
      (write (ldb (byte 4 52) int) :base 16 :stream outstr)
      (write (ldb (byte 4 48) int) :base 16 :stream outstr)
      (write ":" :stream outstr :escape nil)
      (write (ldb (byte 4 44) int) :base 16 :stream outstr)
      (write (ldb (byte 4 40) int) :base 16 :stream outstr)
      (write (ldb (byte 4 36) int) :base 16 :stream outstr)
      (write (ldb (byte 4 32) int) :base 16 :stream outstr)
      (write ":" :stream outstr :escape nil)
      (write (ldb (byte 4 28) int) :base 16 :stream outstr)
      (write (ldb (byte 4 24) int) :base 16 :stream outstr)
      (write (ldb (byte 4 20) int) :base 16 :stream outstr)
      (write (ldb (byte 4 16) int) :base 16 :stream outstr)
      (write ":" :stream outstr :escape nil)
      (write (ldb (byte 4 12) int) :base 16 :stream outstr)
      (write (ldb (byte 4 8) int) :base 16 :stream outstr)
      (write (ldb (byte 4 4) int) :base 16 :stream outstr)
      (write (ldb (byte 4 0) int) :base 16 :stream outstr))))
