#lang racket

(provide utm-zone-tests)

(require "utm.rkt")
(require rackunit)

;; (: utm-zone-tests TestSuite)
(define utm-zone-tests
  (test-suite
    "UTM Zone Tests"
    (check-equal? (utm-zone -55.371094 46.794044) 21)
    (check-equal? (utm-zone -18.105469 65.105450) 27)
    (check-equal? (utm-zone 110.390625 64.616345) 49)
    (check-equal? (utm-zone 28.476563  64.990511) 35)))

