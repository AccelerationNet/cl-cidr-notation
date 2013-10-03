# cl-cidr-notation

A Common Lisp library for converting ip addresses and CIDR blocks
from integer to string representations and vice versa

The functions are optimized for speed.

EG: "1.2.3.4" => 16909060 , 32
EG: "1.2.3.4/30" => 16909060 , 30

## cidr-string, ip-string

Convert integers to strings

## range-string

Given two integer ips, produce a "0.0.0.0-0.0.0.1" representation

## parse-cidr, parse-ip 

Convert strings into integers, throwing meaningful cidr-parse-error if
it fails


## Speed Results
This is probably not super meaningful, but at least gives some benchmarks for the current speeds Im seeing

```
CL-CIDR-NOTATION-TEST> (time (loop for i from 0 below (expt 2 32) by (expt 2 8)
                                 do (ip-string i)))
Evaluation took:
  4.776 seconds of real time
  4.764298 seconds of total run time (4.724295 user, 0.040003 system)
  [ Run times consist of 1.344 seconds GC time, and 3.421 seconds non-GC time. ]
  99.75% CPU
  11,908,350,405 processor cycles
  1,610,611,712 bytes consed

CL-CIDR-NOTATION-TEST> (time (loop for i from 0 below (expt 2 32) by (expt 2 8)
                                   do (parse-ip (ip-string i))))
Evaluation took:
  17.223 seconds of real time
  17.029064 seconds of total run time (16.961060 user, 0.068004 system)
  [ Run times consist of 1.380 seconds GC time, and 15.650 seconds non-GC time. ]
  98.87% CPU
  42,948,143,182 processor cycles
  1,610,612,736 bytes consed
```

## Authors
 * Stassats @ #lisp - super optimization work
 * Russ Tyndall - Acceleration.net
 * Nathan Bird - Acceleration.net