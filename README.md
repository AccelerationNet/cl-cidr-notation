# cl-cidr-notation

A Common Lisp library for converting IP addresses and CIDR blocks from integer to string representations, and vice versa.

The IPv4 functions are optimized for speed.

EG:
"1.2.3.4" => 16909060, 32
"1.2.3.4/30" => 16909060 , 30
"::1" => 1, 128
"cafe:beef::/64" => 269826771143976387270773007078999982080, 64

This library was spawned by this paste: http://paste.lisp.org/display/139246

## cidr-string, ip-string, ipv6-string

Convert integers to strings.

## range-string

Given two integer IPv4 addresses, produce a "0.0.0.0-0.0.0.1" representation.

## parse-cidr, parse-ip, parse-ipv6-cidr, parse-ipv6

Convert strings into integers, throwing meaningful cidr-parse-error on failure.


## Speed Results for IPv4 conversions
This is probably not super meaningful, but at least gives some benchmarks for the current speeds I'm seeing

```
CL-CIDR-NOTATION-TEST> (time (loop for i from 0 below (expt 2 32) by (expt 2 8)
                                 do (ip-string i)))
Evaluation took:
  2.233 seconds of real time
  2.216139 seconds of total run time (2.184137 user, 0.032002 system)
  [ Run times consist of 0.248 seconds GC time, and 1.969 seconds non-GC time. ]
  99.24% CPU
  5,568,010,455 processor cycles
  536,821,744 bytes consed

CL-CIDR-NOTATION-TEST> (time (loop for i from 0 below (expt 2 32) by (expt 2 8)
                                   do (parse-ip (ip-string i))))
Evaluation took:
  11.253 seconds of real time
  11.204700 seconds of total run time (11.040690 user, 0.164010 system)
  [ Run times consist of 0.416 seconds GC time, and 10.789 seconds non-GC time. ]
  99.57% CPU
  28,062,521,077 processor cycles
  536,866,272 bytes consed
```

## Authors
 * Stas Boukarev - stassats @ #lisp - super optimization work
 * Russ Tyndall - Acceleration.net
 * Nathan Bird - Acceleration.net
 * James Fleming - james@electronic-quill.net - unoptimised IPv6 additions
