(use-modules
 (srfi srfi-1)
 (srfi srfi-9)
 (srfi srfi-9 gnu)
 (srfi srfi-11)

 (ice-9 match)

 (chickadee)
 (chickadee graphics color)
 (chickadee graphics path)
 (chickadee graphics text)
 (chickadee math vector)
 (chickadee scripting)

 (quad-tree))

;;; Records.

(define-record-type <point>
  (make-point position velocity)
  point?
  (position point-position set-point-position!)
  (velocity point-velocity set-point-velocity!))

;;; Globals.

(define *debug* #f)

(define *use-quad-tree?* #f)

(define TAU (* 4 (acos 0)))

(define *window-width* 750)
(define *window-height* 750)

(define *arena-width* 1)
(define *arena-height* 1)

;; XXX: A lot bigger than the arena because points may go beyond arena bounds.
(define *quad-tree-region* (make-region #:x-low -10 #:x-high 10
                                        #:y-low -10 #:y-high 10))

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

(define (turn-at-square-edges boid)
  (let* ([x (real-part (point-position boid))]
         [y (imag-part (point-position boid))]
         [velocity (point-velocity boid)]
         [velocity-x (real-part velocity)]
         [velocity-y (imag-part velocity)]
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
    (set-point-velocity! boid (+ velocity
                                 (* velocity-x-addition 1+0i)
                                 (* velocity-y-addition 0+1i)))))

(define (in-range? boid-a boid-b range)
  (define (square x)
    (* x x))
  (let* ([position-a (point-position boid-a)]
         [position-b (point-position boid-b)]
         [a-sub-b (- position-a position-b)])
    (< (+ (square (real-part a-sub-b))
          (square (imag-part a-sub-b)))
       (square range))))

(define (update-boid boid-me other-boids)
  (debug-format "update-boid: before: velocity: ~s~%" (point-velocity boid-me))
  (let ([boid-me-center (point-position boid-me)]
        [boid-me-velocity (point-velocity boid-me)])
    (let loop ([other-boids other-boids]
               [sum-of-differences-in-position-with-other-boids 0.0+0.0i]
               [sum-of-boid-velocities-in-visual-range 0.0+0.0i]
               [sum-of-boid-positions-in-visual-range 0.0+0.0i]
               [number-of-boids-in-visual-range 0])
      (match other-boids
        ['()
         (let ( ;; Separation.
               [new-boid-me-velocity (+ boid-me-velocity
                                        ;; Separation.
                                        (* sum-of-differences-in-position-with-other-boids
                                           (boids-parameters-avoid-factor *boids-parameters*)))])
           (cond
            [(zero? number-of-boids-in-visual-range)
             (set-point-velocity! boid-me new-boid-me-velocity)]
            [else
             (let* ( ;; Alignment.
                    [average-of-boids-velocities-in-visual-range
                     (/ sum-of-boid-velocities-in-visual-range
                        number-of-boids-in-visual-range)]
                    ;; Cohesion.
                    [average-of-other-boid-positions-in-visual-range
                     (/ sum-of-boid-positions-in-visual-range
                        number-of-boids-in-visual-range)]
                    [new-boid-me-velocity (+ new-boid-me-velocity
                                             ;; Alignment.
                                             (* (- average-of-boids-velocities-in-visual-range
                                                   new-boid-me-velocity)
                                                (boids-parameters-matching-factor *boids-parameters*))
                                             ;; Cohesion.
                                             (* (- average-of-other-boid-positions-in-visual-range
                                                   boid-me-center)
                                                (boids-parameters-centering-factor *boids-parameters*))
                                             )])
               (set-point-velocity! boid-me new-boid-me-velocity))]))]
        [(other-boid . rest-of-other-boids)
         (cond
          ;; Nothing to do if other-boid is me.
          [(eq? boid-me other-boid)
           (debug-format "update-boid: eq boid-me other-boid.~%")
           (loop
            rest-of-other-boids
            sum-of-differences-in-position-with-other-boids
            sum-of-boid-velocities-in-visual-range
            sum-of-boid-positions-in-visual-range
            number-of-boids-in-visual-range)]
          ;; If other-boid is in protected range, update all things.
          [(in-range? boid-me other-boid (boids-parameters-protected-range *boids-parameters*))
           (debug-format "update-boid: protected range~%")
           (loop
            rest-of-other-boids
            (+ sum-of-differences-in-position-with-other-boids
               (- boid-me-center
                  (point-position other-boid)))
            (+ sum-of-boid-velocities-in-visual-range
               (point-velocity other-boid))
            (+ sum-of-boid-positions-in-visual-range
               (point-position other-boid))
            (1+ number-of-boids-in-visual-range)
            )]
          ;; If other-boid is not within the protected range but is in
          ;; the visual range, only update the alignment and cohesion stuff.
          [(in-range? boid-me other-boid (boids-parameters-visual-range *boids-parameters*))
           (debug-format "update-boid: only visual range~%")
           (loop
            rest-of-other-boids
            sum-of-differences-in-position-with-other-boids
            (+ sum-of-boid-velocities-in-visual-range
               (point-velocity other-boid))
            (+ sum-of-boid-positions-in-visual-range
               (point-position other-boid))
            (1+ number-of-boids-in-visual-range)
            )]
          ;; or when other-boid is outside all ranges.
          [else
           (debug-format "update-boid: not in range~%")
           (loop
            rest-of-other-boids
            sum-of-differences-in-position-with-other-boids
            sum-of-boid-velocities-in-visual-range
            sum-of-boid-positions-in-visual-range
            number-of-boids-in-visual-range)
           ]
          )])))
  (debug-format "update-boid: after: velocity: ~s~%" (point-velocity boid-me)))

(define (enforce-boid-speed-limits boid)
  (define (clip low value high)
    (max low (min value high)))
  (define (normalize-complex-number complex-number)
    (let ((our-magnitude (magnitude complex-number)))
      (cond
       ((zero? our-magnitude)
        0)
       (else
        (/ complex-number
           our-magnitude)))))
  (let* ((old-velocity (point-velocity boid))
         (clipped-speed (clip (boids-parameters-minimal-speed *boids-parameters*)
                              (magnitude old-velocity)
                              (boids-parameters-maximal-speed *boids-parameters*)))
         (new-velocity  (* clipped-speed
                           (normalize-complex-number old-velocity)))
         )
    (debug-format "enforce-boid-speed-limits: old-velocity: ~A~%" old-velocity)
    (debug-format "enforce-boid-speed-limits: clipped-speed: ~A~%" clipped-speed)
    (debug-format "enforce-boid-speed-limits: new-velocity: ~A~%" new-velocity)
    (set-point-velocity! boid new-velocity)))

(define (make-points n)
  (let ((number-of-points n))
    (unfold
     (lambda (seed) (>= seed number-of-points))
     (lambda (seed)
       (make-point (make-rectangular (* *arena-width* (random:uniform))
                                     (* *arena-height* (random:uniform)))
                   (make-polar 0.01 (* TAU (random:uniform)))))
     1+
     0)))

(define *points* (make-points 10))

(define *quad-tree*
  (quad-tree-insert-list
   (make-quad-tree *quad-tree-region* #:bucket-size 4)
   *points*
   (lambda (point)
     (let ((x (real-part (point-position point)))
           (y (imag-part (point-position point))))
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
  (let* ((old-position (point-position point))
         (old-velocity (point-velocity point))
         (old-dx (real-part old-velocity))
         (old-dy (imag-part old-velocity))
         (new-position (+ old-position old-velocity))
         (new-x (real-part new-position))
         (new-y (imag-part new-position)))
    (let-values (((new-x new-dx) (step-bump new-x old-dx))
                 ((new-y new-dy) (step-bump new-y old-dy)))
      (set-point-position! point (make-rectangular new-x new-y))
      (set-point-velocity! point (make-rectangular new-dx new-dy)))))

(define (update-quad-tree)
  (set! *quad-tree*
        (quad-tree-insert-list
         (make-quad-tree *quad-tree-region* #:bucket-size 4)
         *points*
         (lambda (point)
           (values (real-part (point-position point))
                   (imag-part (point-position point)))))))

(define (update dt)
  (debug-format "~A ~A~%" dt (elapsed-time))

  (update-quad-tree)

  (for-each (lambda (point)
              (cond
               (*use-quad-tree?*
                (let ((visual-range-around-self (make-circle (real-part (point-position point))
                                                             (imag-part (point-position point))
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
              (set-point-position! point (+ (point-position point)
                                            (point-velocity point))))
            *points*)
  )

;;; draw.

(define (draw alpha)
  (let ((width (window-width (current-window)))
        (height (window-height (current-window))))
    (let ((result-items (quad-tree-locate-area
                         *quad-tree*
                         (lambda (quadrant)
                           (region-intersects-region? quadrant *quad-tree-region*))
                         (lambda (x y)
                           (coordinate-in-region? x y *quad-tree-region*))))
          (boundaries (quad-tree-boundaries *quad-tree*)))
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
                                            (circle (vec2 (* width (real-part (point-position point)))
                                                          (* height (imag-part (point-position point))))
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

(run-game
 #:window-width *window-width*
 #:window-height *window-height*
 #:update update
 #:draw draw
 #:key-press key-press)
