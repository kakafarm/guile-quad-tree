(use-modules
 (srfi srfi-1)
 (srfi srfi-64)

 (ice-9 match)
 (ice-9 pretty-print)

 (quad-tree)
 )

(define *debug* #f)

(define format (lambda (output-port template . arguments)
                 (when *debug*
                   (apply format output-port template arguments)
                   )))

(define (random-between-low-and-high low high)
  "Return a random number of a uniform discrete distribution between @var{low} inclusive and @var{high} exclusive."
  (+ (random (- high low)) low))

(define (list-shuffle lst)
  (let* ((our-vector (list->vector lst))
         (our-vector-length (vector-length our-vector))
         (last-index (1- our-vector-length)))
    (let loop ((current-index 0))
      (cond
       ((>= current-index our-vector-length)
        (vector->list our-vector))
       (else
        (let* ((random-index (random-between-low-and-high current-index
                                                          our-vector-length))
               (value-at-random-index (vector-ref our-vector random-index))
               (value-at-current-index (vector-ref our-vector current-index)))
          (vector-set! our-vector
                       current-index
                       value-at-random-index)
          (vector-set! our-vector
                       random-index
                       value-at-current-index)
          (loop (1+ current-index))))))))

(define* (linspace-1d #:key low high number)
  (when (< number 2) (error "number must be 2 or higher." number))
  (unfold
   (lambda (i)
     (>= i number))
   (lambda (i)
     (let ((ratio (/ i (1- number))))
       (+ (* low (- 1 ratio))
          (* high ratio))))
   1+
   0))

(test-begin "QUAD-TREE")

(define unit-square-region (make-region #:x-low 0.0
                                        #:x-high 1.0
                                        #:y-low 0.0
                                        #:y-high 1.0))

(define empty-quad-tree (make-quad-tree unit-square-region))

(define position-x (/ 2))
(define position-y (/ 2))
(define value 'poop)

(define quad-tree (quad-tree-insert
                   (quad-tree-insert empty-quad-tree
                                     position-x
                                     position-y
                                     'poop)
                   (/ 3)
                   (/ 3)
                   'moo))

(format #t "quad-tree: ~S~%" quad-tree)

(define (make-item denominator)
  (let* ((enumerator-x (random (1+ denominator)))
         (enumerator-y (random (1+ denominator)))
         (x (/ enumerator-x
               denominator))
         (y (/ enumerator-y
               denominator)))
    (list x y (map exact->inexact (list x y)))
    ))

(define fifty-items
  (let loop ((n 50)
             (items '()))
    (cond
     ((= 0 n)
      items)
     (else
      (loop
       (1- n)
       (cons (make-item 100) items))))))

(define fifty-item-tree
  (let loop ((quad-tree (make-quad-tree unit-square-region))
             (items fifty-items))
    (match items
      ('() quad-tree)
      (((x y value) . rest-of-items)
       (loop (quad-tree-insert quad-tree
                               x
                               y
                               value)
             rest-of-items)))))

(format #t "empty tree: ~S~%" empty-quad-tree)
(format #t "location in empty tree: ~S~%" (quad-tree-locate-position empty-quad-tree
                                                                     position-x
                                                                     position-y))
(format #t "tree: ~S~%" quad-tree)
(format #t "existing location in tree: ~S~%" (quad-tree-locate-position quad-tree
                                                                        position-x
                                                                        position-y
                                                                        ))

(format #t "fifty item tree: ~S~%" fifty-item-tree)

(test-group "quad-tree-locate-position"
  (test-equal #f
    (quad-tree-locate-position empty-quad-tree
                               position-x
                               position-y))

  (test-equal value
    (quad-tree-locate-position quad-tree
                               position-x
                               position-y))

  (for-each (lambda (item)
              (test-equal (caddr item)
                (quad-tree-locate-position fifty-item-tree
                                           (car item)
                                           (cadr item))))
            fifty-items))

(test-group "quad-tree-locate-region"
  (let* ((number-of-points 50)
         (coordinates-x (linspace-1d #:low 0.0
                                     #:high 1.0
                                     #:number number-of-points))
         (coordinates-y (linspace-1d #:low 0.0
                                     #:high 1.0
                                     #:number number-of-points))
         (test-region (make-region #:x-low (/ 3.0 5)
                                   #:x-high (/ 4.0 5)
                                   #:y-low (/ 1.0 3)
                                   #:y-high (/ 2.0 3)))
         (coordinates
          (zip (concatenate (map (lambda (x) (make-list number-of-points x)) coordinates-x))
               (concatenate (make-list number-of-points coordinates-y))))
         (quad-tree
          (quad-tree-insert-list (make-quad-tree unit-square-region)
                                 (list-shuffle coordinates)
                                 (lambda (coordinate) (apply values coordinate))))
         (items-in-region-regular
          (match-let ((($ <region> x-low x-high y-low y-high) test-region))
            (filter (match-lambda ((x y)
                                  (and (<= x-low x x-high)
                                       (<= y-low y y-high))))
                    (list-shuffle coordinates))))
         (items-in-region-quad-tree
          (quad-tree-locate-area quad-tree
                                 (lambda (quadrant)
                                   (region-intersects-region? quadrant test-region))
                                 (lambda (x y)
                                   (coordinate-in-region? x y test-region)))))
    (test-assert (lset= equal?
                        items-in-region-regular
                        items-in-region-quad-tree))))

(test-group "quad-tree-locate-circle"
  (let* ((number-of-points 50)
         (coordinates-x (linspace-1d #:low 0.0
                                     #:high 1.0
                                     #:number number-of-points))
         (coordinates-y (linspace-1d #:low 0.0
                                     #:high 1.0
                                     #:number number-of-points))
         (test-circle (make-circle 0.5 0.5 0.2))
         (coordinates
          (zip (concatenate (map (lambda (x) (make-list number-of-points x)) coordinates-x))
               (concatenate (make-list number-of-points coordinates-y))))
         (shuffled-coordinates (list-shuffle coordinates))
         (quad-tree (quad-tree-insert-list (make-quad-tree unit-square-region)
                                           shuffled-coordinates
                                           (lambda (coordinate) (apply values coordinate))))
         (items-in-circle-regular
          (filter (lambda (coordinate)
                    (match-let (((x y) coordinate))
                      (coordinate-in-circle? x y test-circle)))
                  shuffled-coordinates))
         (items-in-circle-quad-tree
          (quad-tree-locate-area quad-tree
                                 (lambda (quadrant)
                                   (region-intersects-circle? quadrant test-circle))
                                 (lambda (x y)
                                   (coordinate-in-circle? x y test-circle)))))
    (test-assert (lset= equal?
                        items-in-circle-regular
                        items-in-circle-quad-tree))))

(test-end)
