(use-modules
 (srfi srfi-1)
 (srfi srfi-9)
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

(define *width* 250)

(define *height* 250)

(define *number-of-points* 10)

(define *boids-turn-factor* (make-parameter 0.1))
(define *boids-visual-range* (make-parameter 0.1 #; (* 4 *enemy-size*)))
(define *boids-protected-range* (make-parameter 0.05 #; *enemy-size*))
(define *boids-centering-factor* (make-parameter 0.1))
(define *boids-avoid-factor* (make-parameter 0.1))
(define *boids-matching-factor* (make-parameter 0.1))
(define *boids-minimal-speed* (make-parameter 0.01 #; (* 0.5 *enemy-speed*)))
(define *boids-maximal-speed* (make-parameter 0.005 #; *enemy-speed*))
(define *boids-maximal-bias* (make-parameter 0.1))
(define *boids-bias-increment* (make-parameter 0.0001))

(define *region* (make-region #:x-low (/ 3 5) #:x-high (/ 4 5)
                              #:y-low (/ 1 3) #:y-high (/ 2 3)))

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
            (*boids-turn-factor*)]
           [(> x 1)
            (- (*boids-turn-factor*))]
           [else
            0.0])]
         [velocity-y-addition
          (cond
           [(< y 0)
            (*boids-turn-factor*)]
           [(> y 1)
            (- (*boids-turn-factor*))]
           [else
            0.0])])
    (set-point-velocity! boid (+ velocity
                                 (* velocity-x-addition 1+0i)
                                 (* velocity-y-addition 0+1i)))))

(define (in-range? boid-a boid-b range)
  (let* ([position-a (point-position boid-a)]
         [position-b (point-position boid-b)]
         [a-sub-b (- position-a position-b)])
    (< (+ (expt (real-part a-sub-b) 2)
          (expt (imag-part a-sub-b) 2))
       (expt range 2))))

(define (update-boid-regular boid-me other-boids)
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
                                           (*boids-avoid-factor*)))])
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
                                                (*boids-matching-factor*))
                                             ;; Cohesion.
                                             (* (- average-of-other-boid-positions-in-visual-range
                                                   boid-me-center)
                                                (*boids-centering-factor*))
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
          [(in-range? boid-me other-boid (*boids-protected-range*))
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
          [(in-range? boid-me other-boid (*boids-visual-range*))
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

(define (update-boid-quad-tree boid-me boids-quad-tree)
  (debug-format "update-boid: before: velocity: ~s~%" (point-velocity boid-me))
  (let* ([boid-me-center (point-position boid-me)]
         [boid-me-velocity (point-velocity boid-me)]
         [boid-me-x (real-part boid-me-center)]
         [boid-me-y (real-part boid-me-center)]
         [visual-range-around-boid-me (make-circle boid-me-x
                                                   boid-me-y
                                                   (*boids-visual-range*))])
    (let loop ([other-boids (quad-tree-locate-area
                             boids-quad-tree
                             (lambda (quadrant)
                               (region-intersects-circle? quadrant
                                                          visual-range-around-boid-me))
                             (lambda (x y)
                               (coordinate-in-circle?
                                x
                                y
                                visual-range-around-boid-me)))]
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
                                           (*boids-avoid-factor*)))])
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
                                                (*boids-matching-factor*))
                                             ;; Cohesion.
                                             (* (- average-of-other-boid-positions-in-visual-range
                                                   boid-me-center)
                                                (*boids-centering-factor*))
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
          [(in-range? boid-me other-boid (*boids-protected-range*))
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
          [(in-range? boid-me other-boid (*boids-visual-range*))
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
         (clipped-speed (clip (*boids-minimal-speed*)
                              (magnitude old-velocity)
                              (*boids-maximal-speed*)))
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
       (make-point (make-rectangular (random:uniform) (random:uniform))
                   (make-polar 0.01 (* TAU (random:uniform)))))
     1+
     0)))

(define *points* (make-points 10))

(define *quad-tree*
  (quad-tree-insert-list
   (make-quad-tree
    (make-region #:x-low -10
                 #:x-high 10
                 #:y-low -10
                 #:y-high 10))
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
         (make-quad-tree (make-region #:x-low -10
                                      #:x-high 10
                                      #:y-low -10
                                      #:y-high 10))
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
                (update-boid-quad-tree point *quad-tree*))
               (else
                (update-boid-regular point *points*)))
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
                           (region-intersects-region? quadrant *region*))
                         (lambda (x y)
                           (coordinate-in-region? x y *region*))))
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
                               (stroke-width 0.1))
                              (apply stroke
                                     (map (lambda (point)
                                            (circle (vec2 (* width (real-part (point-position point)))
                                                          (* height (imag-part (point-position point))))
                                                    1))
                                          *points*)))))))

;;; key-press.

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
 #:window-width *width*
 #:window-height *height*
 #:update update
 #:draw draw
 #:key-press key-press)
