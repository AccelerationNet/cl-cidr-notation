# cl-cidr-notation

A Common Lisp library for converting ip addresses and CIDR blocks
from integer to string representations and vice versa

EG: "1.2.3.4" => 16909060 , 32
EG: "1.2.3.4/30" => 16909060 , 30

## cidr-string, ip-string

Convert integers to strings

## parse-cidr, parse-ip 

Convert strings into integers, throwing meaningful cidr-parse-error if
it fails

