(use-modules
 (srfi srfi-1)
 (srfi srfi-9)
 (srfi srfi-9 gnu)
 (srfi srfi-11)

 (ice-9 match)

 (statprof)

 (chickadee)
 (chickadee graphics color)
 (chickadee graphics path)
 (chickadee graphics text)
 (chickadee math vector)
 (chickadee scripting)

 (quad-tree))

;;; Records.

(define-record-type <point>
  (make-point position-x position-y velocity-x velocity-y)
  point?
  (position-x point-position-x set-point-position-x!)
  (position-y point-position-y set-point-position-y!)
  (velocity-x point-velocity-x set-point-velocity-x!)
  (velocity-y point-velocity-y set-point-velocity-y!))

;;; Globals.

(define *debug* #f)

(define *use-quad-tree?* #f)

(define TAU (* 4 (acos 0)))

(define *window-width* 750)
(define *window-height* 750)

(define *arena-width* 1)
(define *arena-height* 1)

;; XXX: A lot bigger than the arena because points may go beyond arena bounds.
(define *quad-tree-region* (make-region #:x-low -10.0 #:x-high 10.0
                                        #:y-low -10.0 #:y-high 10.0))

(define *number-of-points* 10)

(define-immutable-record-type <boids-parameters>
  (make-boids-parameters
   turn-factor
   visual-range
   protected-range
   centering-factor
   avoid-factor
   matching-factor
   minimal-speed
   maximal-speed
   )
  boids-parameters?
  (turn-factor boids-parameters-turn-factor set-boids-parameters-turn-factor)
  (visual-range boids-parameters-visual-range set-boids-parameters-visual-range)
  (protected-range boids-parameters-protected-range set-boids-parameters-protected-range)
  (centering-factor boids-parameters-centering-factor set-boids-parameters-centering-factor)
  (avoid-factor boids-parameters-avoid-factor set-boids-parameters-avoid-factor)
  (matching-factor boids-parameters-matching-factor set-boids-parameters-matching-factor)
  (minimal-speed boids-parameters-minimal-speed set-boids-parameters-minimal-speed)
  (maximal-speed boids-parameters-maximal-speed set-boids-parameters-maximal-speed)
  (maximal-bias boids-parameters-maximal-bias set-boids-parameters-maximal-bias))

(define *boids-parameters* (make-boids-parameters 0.1 0.1 0.06 0.1 0.1 0.1 0.005 0.01))

(define *fold-increase-factor* (/ 6 5))

(define* (debug-format x #:rest stuff)
  (if *debug*
      (apply format #t x stuff)
      #f))

(define (square x)
  (* x x))

(define (turn-at-square-edges boid)
  (let* ([x (point-position-x boid)]
         [y (point-position-y boid)]
         [velocity-x (point-velocity-x boid)]
         [velocity-y (point-velocity-y boid)]
         [velocity-x-addition
          (cond
           [(< x 0)
            (boids-parameters-turn-factor *boids-parameters*)]
           [(> x 1)
            (- (boids-parameters-turn-factor *boids-parameters*))]
           [else
            0.0])]
         [velocity-y-addition
          (cond
           [(< y 0)
            (boids-parameters-turn-factor *boids-parameters*)]
           [(> y 1)
            (- (boids-parameters-turn-factor *boids-parameters*))]
           [else
            0.0])])
    (set-point-velocity-x! boid (+ velocity-x velocity-x-addition))
    (set-point-velocity-y! boid (+ velocity-y velocity-y-addition))))

(define (in-range? boid-a boid-b range-squared)
  (unless (and (real? range-squared) (inexact? range-squared))
    (error "All arguments must be floats:" range-squared))
  (let* ([position-a-x (point-position-x boid-a)]
         [position-a-y (point-position-y boid-a)]
         [position-b-x (point-position-x boid-b)]
         [position-b-y (point-position-y boid-b)]
         [a-sub-b-x (- position-a-x position-b-x)]
         [a-sub-b-y (- position-a-y position-b-y)])
    (< (+ (square a-sub-b-x)
          (square a-sub-b-y))
       range-squared)))

(define (update-boid boid-me other-boids)
  (debug-format "update-boid: before: velocity: ~s ~s~%"
                (point-velocity-x boid-me)
                (point-velocity-y boid-me)
                )
  (let ([boid-me-center-x (point-position-x boid-me)]
        [boid-me-center-y (point-position-y boid-me)]
        [boid-me-velocity-x (point-velocity-x boid-me)]
        [boid-me-velocity-y (point-velocity-y boid-me)]
        [protected-range-squared (square (boids-parameters-protected-range *boids-parameters*))]
        [visual-range-squared (square (boids-parameters-visual-range *boids-parameters*))])
    (let loop ([other-boids other-boids]
               [sum-of-differences-in-position-with-other-boids-x 0.0]
               [sum-of-differences-in-position-with-other-boids-y 0.0]
               [sum-of-boid-velocities-in-visual-range-x 0.0]
               [sum-of-boid-velocities-in-visual-range-y 0.0]
               [sum-of-boid-positions-in-visual-range-x 0.0]
               [sum-of-boid-positions-in-visual-range-y 0.0]
               [number-of-boids-in-visual-range 0])
      (match other-boids
        ['()
         (let ( ;; Separation.
               [new-boid-me-velocity-x (+ boid-me-velocity-x
                                          ;; Separation.
                                          (* sum-of-differences-in-position-with-other-boids-x
                                             (boids-parameters-avoid-factor *boids-parameters*)))]
               [new-boid-me-velocity-y (+ boid-me-velocity-y
                                          ;; Separation.
                                          (* sum-of-differences-in-position-with-other-boids-y
                                             (boids-parameters-avoid-factor *boids-parameters*)))])
           (cond
            [(zero? number-of-boids-in-visual-range)
             (set-point-velocity-x! boid-me new-boid-me-velocity-x)
             (set-point-velocity-y! boid-me new-boid-me-velocity-y)]
            [else
             (let* ( ;; Alignment.
                    [average-of-boids-velocities-in-visual-range-x
                     (/ sum-of-boid-velocities-in-visual-range-x
                        number-of-boids-in-visual-range)]
                    [average-of-boids-velocities-in-visual-range-y
                     (/ sum-of-boid-velocities-in-visual-range-y
                        number-of-boids-in-visual-range)]
                    ;; Cohesion.
                    [average-of-other-boid-positions-in-visual-range-x
                     (/ sum-of-boid-positions-in-visual-range-x
                        number-of-boids-in-visual-range)]
                    [average-of-other-boid-positions-in-visual-range-y
                     (/ sum-of-boid-positions-in-visual-range-y
                        number-of-boids-in-visual-range)]
                    [new-boid-me-velocity-x (+ new-boid-me-velocity-x
                                             ;; Alignment.
                                             (* (- average-of-boids-velocities-in-visual-range-x
                                                   new-boid-me-velocity-x)
                                                (boids-parameters-matching-factor *boids-parameters*))
                                             ;; Cohesion.
                                             (* (- average-of-other-boid-positions-in-visual-range-x
                                                   boid-me-center-x)
                                                (boids-parameters-centering-factor *boids-parameters*))
                                             )]
                    [new-boid-me-velocity-y (+ new-boid-me-velocity-y
                                             ;; Alignment.
                                             (* (- average-of-boids-velocities-in-visual-range-y
                                                   new-boid-me-velocity-y)
                                                (boids-parameters-matching-factor *boids-parameters*))
                                             ;; Cohesion.
                                             (* (- average-of-other-boid-positions-in-visual-range-y
                                                   boid-me-center-y)
                                                (boids-parameters-centering-factor *boids-parameters*))
                                             )])
               (set-point-velocity-x! boid-me new-boid-me-velocity-x)
               (set-point-velocity-y! boid-me new-boid-me-velocity-y))]))]
        [(other-boid . rest-of-other-boids)
         (cond
          ;; Nothing to do if other-boid is me.
          [(eq? boid-me other-boid)
           (debug-format "update-boid: eq boid-me other-boid.~%")
           (loop
            rest-of-other-boids
            sum-of-differences-in-position-with-other-boids-x
            sum-of-differences-in-position-with-other-boids-y            
            sum-of-boid-velocities-in-visual-range-x
            sum-of-boid-velocities-in-visual-range-y
            sum-of-boid-positions-in-visual-range-x
            sum-of-boid-positions-in-visual-range-y
            number-of-boids-in-visual-range)]
          ;; If other-boid is in protected range, update all things.
          [(in-range? boid-me other-boid protected-range-squared)
           (debug-format "update-boid: protected range~%")
           (loop
            rest-of-other-boids
            (+ sum-of-differences-in-position-with-other-boids-x
               (- boid-me-center-x
                  (point-position-x other-boid)))
            (+ sum-of-differences-in-position-with-other-boids-y
               (- boid-me-center-y
                  (point-position-y other-boid)))
            (+ sum-of-boid-velocities-in-visual-range-x
               (point-velocity-x other-boid))
            (+ sum-of-boid-velocities-in-visual-range-y
               (point-velocity-y other-boid))
            (+ sum-of-boid-positions-in-visual-range-x
               (point-position-x other-boid))
            (+ sum-of-boid-positions-in-visual-range-y
               (point-position-y other-boid))
            (1+ number-of-boids-in-visual-range)
            )]
          ;; If other-boid is not within the protected range but is in
          ;; the visual range, only update the alignment and cohesion stuff.
          [(in-range? boid-me other-boid visual-range-squared)
           (debug-format "update-boid: only visual range~%")
           (loop
            rest-of-other-boids
            sum-of-differences-in-position-with-other-boids-x
            sum-of-differences-in-position-with-other-boids-y

            (+ sum-of-boid-velocities-in-visual-range-x
               (point-velocity-x other-boid))
            (+ sum-of-boid-velocities-in-visual-range-y
               (point-velocity-y other-boid))

            (+ sum-of-boid-positions-in-visual-range-x
               (point-position-x other-boid))
            (+ sum-of-boid-positions-in-visual-range-y
               (point-position-y other-boid))
            
            (1+ number-of-boids-in-visual-range)
            )]
          ;; or when other-boid is outside all ranges.
          [else
           (debug-format "update-boid: not in range~%")
           (loop
            rest-of-other-boids
            sum-of-differences-in-position-with-other-boids-x
            sum-of-differences-in-position-with-other-boids-y
            sum-of-boid-velocities-in-visual-range-x
            sum-of-boid-velocities-in-visual-range-y
            sum-of-boid-positions-in-visual-range-x
            sum-of-boid-positions-in-visual-range-y
            number-of-boids-in-visual-range)
           ]
          )])))
  (debug-format "update-boid: after: velocity: ~s ~s~%"
                (point-velocity-x boid-me)
                (point-velocity-y boid-me)))

(define (enforce-boid-speed-limits boid)
  (define (clip low value high)
    (max low (min value high)))
  (define (normalize-number x our-magnitude)
    (cond
     ((zero? our-magnitude)
      0)
     (else
      (/ x our-magnitude))))
  (let* ((old-velocity-x (point-velocity-x boid))
         (old-velocity-y (point-velocity-y boid))
         (our-magnitude (sqrt (+ (square old-velocity-x)
                                 (square old-velocity-y))))
         (clipped-speed (clip (boids-parameters-minimal-speed *boids-parameters*)
                              our-magnitude
                              (boids-parameters-maximal-speed *boids-parameters*)))
         (new-velocity-x (* clipped-speed
                            (normalize-number old-velocity-x our-magnitude)))
         (new-velocity-y (* clipped-speed
                            (normalize-number old-velocity-y our-magnitude))))
    (debug-format "enforce-boid-speed-limits: old-velocity: ~A ~A~%" old-velocity-x old-velocity-y)
    (debug-format "enforce-boid-speed-limits: clipped-speed: ~A~%" clipped-speed)
    (debug-format "enforce-boid-speed-limits: new-velocity: ~A ~A~%" new-velocity-x new-velocity-y)
    (set-point-velocity-x! boid new-velocity-x)
    (set-point-velocity-y! boid new-velocity-y)))

(define (make-points n)
  (let ((number-of-points n))
    (unfold
     (lambda (seed) (>= seed number-of-points))
     (lambda (seed)
       (let ((velocity (make-polar 0.01 (* TAU (random:uniform)))))
         (make-point (* *arena-width* (random:uniform))
                     (* *arena-height* (random:uniform))
                     (real-part velocity)
                     (imag-part velocity))))
     1+
     0)))

(define *points* (make-points 10))

(define *quad-tree*
  (quad-tree-insert-list
   (make-quad-tree *quad-tree-region* #:bucket-size 4)
   *points*
   (lambda (point)
     (let ((x (point-position-x point))
           (y (point-position-y point)))
       (values x y)))))

;;; update.

(define (step-bump-point point)
  (define (step-bump x speed)
    (cond
     ((< x 0)
      (values (- x)
              (- speed)))
     ((> x 1)
      (values (- 2 x)
              (- speed)))
     (else
      (values x speed))))
  (let* ((old-position-x (point-position-x point))
         (old-position-y (point-position-y point))
         (old-velocity-x (point-velocity-x point))
         (old-velocity-y (point-velocity-y point))
         (new-position-x (+ old-position-x old-velocity-x))
         (new-position-y (+ old-position-y old-velocity-y)))
    (let-values (((new-x new-dx) (step-bump new-position-x old-velocity-x))
                 ((new-y new-dy) (step-bump new-position-y old-velocity-y)))
      (set-point-position-x! point new-x)
      (set-point-position-y! point new-y)
      (set-point-velocity-x! point new-dx)
      (set-point-velocity-y! point new-dy))))

(define (update-quad-tree)
  (set! *quad-tree*
        (quad-tree-insert-list
         (make-quad-tree *quad-tree-region* #:bucket-size 4)
         *points*
         (lambda (point)
           (values (point-position-x point)
                   (point-position-y point))))))

(define (update dt)
  (debug-format "~A ~A~%" dt (elapsed-time))

  (update-quad-tree)

  (for-each (lambda (point)
              (cond
               (*use-quad-tree?*
                (let ((visual-range-around-self (make-circle (point-position-x point)
                                                             (point-position-y point)
                                                             (boids-parameters-visual-range *boids-parameters*))))
                  (update-boid point
                               (quad-tree-locate-area *quad-tree*
                                                      (lambda (quadrant)
                                                        (region-intersects-circle? quadrant
                                                                                   visual-range-around-self))
                                                      (lambda (x y)
                                                        (coordinate-in-circle? x
                                                                               y
                                                                               visual-range-around-self))))))
               (else
                (update-boid point *points*)))
              (turn-at-square-edges point)
              (enforce-boid-speed-limits point)
              (set-point-position-x! point (+ (point-position-x point)
                                              (point-velocity-x point)))
              (set-point-position-y! point (+ (point-position-y point)
                                              (point-velocity-y point))))
            *points*)
  )

;;; draw.

(define (draw alpha)
  (let ((width (window-width (current-window)))
        (height (window-height (current-window))))
    (let ((boundaries (quad-tree-boundaries *quad-tree*)))
      (draw-canvas (make-canvas
                    (with-style ((stroke-color green)
                                 (stroke-width 0.1))
                                (apply stroke
                                       (map (lambda (bounds)
                                              (rectangle (vec2 (* width (region-x-low bounds))
                                                               (* width (region-y-low bounds)))
                                                         (* width (- (region-x-high bounds)
                                                                     (region-x-low bounds)))
                                                         (* height (- (region-y-high bounds)
                                                                      (region-y-low bounds)))))
                                            boundaries))))))
    (draw-canvas (make-canvas
                  (with-style ((stroke-color red)
                               (stroke-width 5))
                              (apply stroke
                                     (map (lambda (point)
                                            (circle (vec2 (* width (point-position-x point))
                                                          (* height (point-position-y point)))
                                                    1))
                                          *points*)))))))

;;; key-press.

(define (fold-decrease get-parameter set-parameter)
  (set! *boids-parameters* (set-parameter *boids-parameters*
                                          (/ (get-parameter *boids-parameters*)
                                             *fold-increase-factor*)))
  (format #t "~A: ~A~%" get-parameter (get-parameter *boids-parameters*)))

(define (fold-increase get-parameter set-parameter)
  (set! *boids-parameters* (set-parameter *boids-parameters*
                                          (* (get-parameter *boids-parameters*)
                                             *fold-increase-factor*)))
  (format #t "~A: ~A~%" get-parameter (get-parameter *boids-parameters*)))

(define (key-press key modifiers repeat?)
  (when (eq? key 'q)
    (abort-game))
  (match key
    ('d
     (display "Debug - ")
     (set! *debug* (not *debug*))
     (display *debug*)
     (newline))
    ('b
     (display "use-quad-tree? - ")
     (set! *use-quad-tree?* (not *use-quad-tree?*))
     (update-quad-tree)
     (display *use-quad-tree?*)
     (newline))
    ('up
     (set! *number-of-points* (* *number-of-points* 2))
     (set! *points* (make-points *number-of-points*))
     (update-quad-tree)
     )
    ('down
     (/ 1 2)
     (set! *number-of-points* (ceiling (/ *number-of-points* 2)))
     (set! *points* (make-points *number-of-points*))
     (update-quad-tree)
     )

    ('t (cond
         ((null? (lset-intersection eq? '(right-shift left-shift) modifiers))
          (fold-increase boids-parameters-turn-factor
                         set-boids-parameters-turn-factor))
         (else (fold-decrease boids-parameters-turn-factor
                              set-boids-parameters-turn-factor))))

    ('v (cond
         ((null? (lset-intersection eq? '(right-shift left-shift) modifiers))
          (fold-increase boids-parameters-visual-range
                         set-boids-parameters-visual-range))
         (else (fold-decrease boids-parameters-visual-range
                              set-boids-parameters-visual-range))))

    ('p (cond
         ((null? (lset-intersection eq? '(right-shift left-shift) modifiers))
          (fold-increase boids-parameters-protected-range
                         set-boids-parameters-protected-range))
         (else (fold-decrease boids-parameters-protected-range
                              set-boids-parameters-protected-range))))

    ('c (cond
         ((null? (lset-intersection eq? '(right-shift left-shift) modifiers))
          (fold-increase boids-parameters-centering-factor
                         set-boids-parameters-centering-factor))
         (else (fold-decrease boids-parameters-centering-factor
                              set-boids-parameters-centering-factor))))

    ('a (cond
         ((null? (lset-intersection eq? '(right-shift left-shift) modifiers))
          (fold-increase boids-parameters-avoid-factor
                         set-boids-parameters-avoid-factor))
         (else (fold-decrease boids-parameters-avoid-factor
                              set-boids-parameters-avoid-factor))))

    ('m (cond ((null? (lset-intersection eq? '(right-shift left-shift) modifiers))
               (fold-increase boids-parameters-matching-factor
                              set-boids-parameters-matching-factor))
              (else (fold-decrease boids-parameters-matching-factor
                                   set-boids-parameters-matching-factor))))

    ('1 (fold-decrease boids-parameters-minimal-speed
                       set-boids-parameters-minimal-speed))
    ('2 (fold-increase boids-parameters-minimal-speed
                       set-boids-parameters-minimal-speed))

    ('3 (fold-decrease boids-parameters-maximal-speed
                       set-boids-parameters-maximal-speed))
    ('4 (fold-increase boids-parameters-maximal-speed
                       set-boids-parameters-maximal-speed))

    (_
     (format #t "key-press:
\tkey: ~S
\tmodifiers: ~S
\trepeat?: ~S~%"
             key
             modifiers
             repeat?))))

;;; run-game.

(statprof-start)
(run-game
 #:window-width *window-width*
 #:window-height *window-height*
 #:update update
 #:draw draw
 #:key-press key-press)
(statprof-stop)

(statprof-display)

(gcprof (lambda () (run-game
                    #:window-width *window-width*
                    #:window-height *window-height*
                    #:update update
                    #:draw draw
                    #:key-press key-press)))
