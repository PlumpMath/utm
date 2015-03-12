#lang typed/racket

(provide utm
         utm-zone)

(: utm-zone (-> Real Real Integer))
(define (utm-zone longitude latitude)
  (cond [(and (>= latitude 56.0)
              (<  latitude 64.0)
              (>= longitude 3.0)
              (<  longitude 12.0))
         32]
        [(and (>= latitude 72.0)
              (<  latitude 84.0)
              (>= longitude 0.0)
              (<  longitude 9.0))
         31]
        [(and (>= latitude 72.0)
              (<  latitude 84.0)
              (>= longitude 9.0)
              (<  longitude 21.0))
         33]
        [(and (>= latitude 72.0)
              (<  latitude 84.0)
              (>= longitude 21.0)
              (<  longitude 33.0))
         35]
        [(and (>= latitude 72.0)
              (<  latitude 84.0)
              (>= longitude 33.0)
              (<  longitude 42.0))
         37]
        [else (+ (floor (inexact->exact (/ (+ longitude 180) 6))) 1)]))

;; Reference:
;; http://gis.stackexchange.com/questions/13291/computing-utm-zone-from-lat-long-point

(: longitude-origin (-> Integer Real))
(define (longitude-origin zone)
  (+ (- (* (- zone 1) 6) 180) 3))

(: utm (-> Real Real (Vector Number Number)))
(define (utm longitude latitude)
  (let* ([a 6378137.0]
         [eccentricity-squared 0.00660438]
         [scale-factor 0.9996]
         [eccentricity-prime-squared
           (/ eccentricity-squared
              (- 1 eccentricity-squared))]
         [N (/ a
               (sqrt (- 1 (* eccentricity-squared
                             (sqr (sin (degrees->radians latitude)))))))]
         [T (sqr (tan (degrees->radians latitude)))]
         [C (* eccentricity-prime-squared
               (cos (degrees->radians latitude))
               (cos (degrees->radians latitude)))]
         [A (* (cos (degrees->radians latitude))
               (- (degrees->radians longitude)
                  (degrees->radians
                    (longitude-origin
                      (utm-zone longitude latitude)))))]
         [M (- (+ (- (* a
                        (* (- 1
                              (/ eccentricity-squared 4)
                              (* 3
                                 eccentricity-squared
                                 eccentricity-squared
                                 (/ 1 64))
                              (- 5
                                 (* eccentricity-squared
                                    eccentricity-squared
                                    eccentricity-squared
                                    (/ 1 256))))
                           (degrees->radians latitude)))
                     (* (+ (* 3
                              eccentricity-squared
                              (/ 1 8))
                           (* 3
                              eccentricity-squared
                              eccentricity-squared
                              (/ 1 32))
                           (* 45
                              eccentricity-squared
                              eccentricity-squared
                              eccentricity-squared
                              (/ 1 1024)))
                        (sin (* 2
                                (degrees->radians latitude)))))
                  (* (+ (* 15
                           eccentricity-squared
                           eccentricity-squared
                           (/ 1 256))
                        (* 45
                           eccentricity-squared
                           eccentricity-squared
                           eccentricity-squared
                           (/ 1 1024)))
                     (sin (* 4
                             (degrees->radians latitude)))))
               (* (* 35
                     eccentricity-squared
                     eccentricity-squared
                     eccentricity-squared
                     (/ 1 3072))
                  (sin (* 6
                          (degrees->radians latitude)))))]
         [easting (+ (* scale-factor
                        N
                        (+ A
                           (* (+ 1
                                 (* -1 T)
                                 C)
                              A
                              A
                              A
                              (/ 1 6))
                           (* (+ 5
                                 (* -18 T)
                                 (* T T)
                                 (* 72 C)
                                 (* -58 eccentricity-prime-squared))
                              A
                              A
                              A
                              A
                              A
                              (/ 1 120))))
                     500000.0)]
         [northing (* scale-factor
                      (+ M
                         (* N
                            (tan (degrees->radians latitude))
                            (+ (* A
                                  (/ A 2))
                               (* (+ 5
                                     (* -1 T)
                                     (* 9 C)
                                     (* 4 C C))
                                  A
                                  A
                                  A
                                  A
                                  (/ 1 24))
                               (* (+ 61
                                     (* -58 T)
                                     (* T T)
                                     (* 600 C)
                                     (* -330 eccentricity-prime-squared))
                                  A
                                  A
                                  A
                                  A
                                  A
                                  A
                                  (/ 1 720))))))])
         (vector easting
                 (if (< latitude 0)
                     (+ 10000000.0 northing)
                     northing))))

;; Reference:
;; http://forum.worldwindcentral.com/showthread.php?9863-C-code-to-convert-DD-to-UTM-here-it-is-!!
