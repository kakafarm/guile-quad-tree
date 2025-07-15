(use-modules
 (srfi srfi-1)
 (srfi srfi-9)
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
  (make-point position velocity)
  point?
  (position point-position set-point-position!)
  (velocity point-velocity set-point-velocity!))

;;; Globals.

(define *debug* #f)

(define *use-quad-tree?* #f)

(define TAU (* 4 (acos 0)))

(define *window-width* 1000)
(define *window-height* 1000)

(define *arena-width* 250)
(define *arena-height* 250)

(define *use-quad-tree* #t)

(define *circle* (make-circle 189 51 109))

(define *region*
  (let* ((x-low (* (/ *arena-width* 2) (random:uniform)))
         (x-high (+ x-low 100))
         (y-low (* (/ *arena-height* 2) (random:uniform)))
         (y-high (+ y-low 100)))
    (make-region #:x-low x-low #:x-high x-high
                 #:y-low y-low #:y-high y-high)))

(define* (debug-format x #:rest stuff)
  (if *debug*
      (apply format #t x stuff)
      #f))

(define *points*
  (let ((number-of-points 80))
    (unfold
     (lambda (seed) (>= seed number-of-points))
     (lambda (seed)
       (make-point (make-rectangular (* *arena-width* (random:uniform))
                                     (* *arena-height* (random:uniform)))
                   (make-polar 2 (* TAU (random:uniform)))))
     1+
     0)))

(define *empty-tree* (make-quad-tree
                      (make-region #:x-low 0 #:x-high *arena-width*
                                   #:y-low 0 #:y-high *arena-height*)))

(define *quad-tree*
  (quad-tree-insert-list
   *empty-tree*
   *points*
   (lambda (point)
     (let ((x (real-part (point-position point)))
           (y (imag-part (point-position point))))
       (values x y)))))

;;; update.

(define (step-bump-point point)
  (define (step-bump x speed border)
    (cond
     ((< x 0)
      (values (- x)
              (- speed)))
     ((> x *arena-width*)
      (values (- (* 2 border) x)
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
    (let-values (((new-x new-dx) (step-bump new-x old-dx *arena-width*))
                 ((new-y new-dy) (step-bump new-y old-dy *arena-height*)))
      (debug-format "step-bump-point: ~s~%" point)
      (set-point-position! point (make-rectangular new-x new-y))
      (set-point-velocity! point (make-rectangular new-dx new-dy)))))

(define (update-quad-tree)
  (set! *quad-tree*
        (quad-tree-insert-list
         *empty-tree*
         *points*
         (lambda (point)
           (values (real-part (point-position point))
                   (imag-part (point-position point)))))))

(define (update dt)
  (debug-format "~A ~A~%" dt (elapsed-time))

  (for-each step-bump-point *points*)

  (update-quad-tree)
  
  (when *use-quad-tree?* (update-quad-tree))
  )

;;; draw.

(define (draw-region region width-ratio height-ratio)
  (draw-canvas (make-canvas (with-style ((stroke-color green)
                                         (stroke-width (* width-ratio 1)))
                                        (stroke (rectangle (vec2 (* width-ratio
                                                                    (region-x-low region))
                                                                 (* height-ratio
                                                                    (region-y-low region)))
                                                           (* width-ratio
                                                              (- (region-x-high region)
                                                                 (region-x-low region)))
                                                           (* height-ratio
                                                              (- (region-y-high region)
                                                                 (region-y-low region)))))))))

(define (draw-circle our-circle width-ratio height-ratio)
  (draw-canvas (make-canvas (with-style ((stroke-color green)
                                         (stroke-width (* width-ratio 1)))
                                        (stroke (circle (vec2 (* width-ratio
                                                                 (circle-x our-circle))
                                                              (* height-ratio
                                                                 (circle-y our-circle)))
                                                        (* width-ratio
                                                           (circle-radius our-circle))))))))

(define (draw-points stroke-color stroke-width points width-ratio height-ratio)
  (draw-canvas (make-canvas
                (with-style ((stroke-color stroke-color)
                             (stroke-width (* width-ratio stroke-width)))
                            (apply stroke
                                   (map (lambda (point)
                                          (circle (vec2 (* width-ratio
                                                           (real-part (point-position point)))
                                                        (* height-ratio
                                                           (imag-part (point-position point))))
                                                  1))
                                        points))))))

(define (draw alpha)
  (let* ((points-in-region-not-circle
          (quad-tree-locate-area
           *quad-tree*
           (lambda (quadrant)
             (and
              (region-intersects-region? *region* quadrant)
              (not (region-subset-of-circle? quadrant *circle*))))
           (lambda (x y)
             (and
              (coordinate-in-region? x y *region*)
              (not (coordinate-in-circle? x y *circle*))))))
         (points-in-circle-not-region
          (quad-tree-locate-area
           *quad-tree*
           (lambda (quadrant)
             (and
              (region-intersects-circle? quadrant *circle*)
              (not (region-subset-of-region? *region* quadrant))))
           (lambda (x y)
             (and
              (coordinate-in-circle? x y *circle*)
              (not (coordinate-in-region? x y *region*))))))
         (points-in-region-and-circle
          (quad-tree-locate-area
           *quad-tree*
           (lambda (quadrant)
             (and
              (region-intersects-circle? quadrant *circle*)
              (region-intersects-region? *region* quadrant)))
           (lambda (x y)
             (and
              (coordinate-in-circle? x y *circle*)
              (coordinate-in-region? x y *region*)))))
         (points-not-in-region-nor-circle
          (quad-tree-locate-area
           *quad-tree*
           (lambda (quadrant)
             (and
              (not (region-subset-of-circle? quadrant *circle*))
              (not (region-subset-of-region? *region* quadrant))))
           (lambda (x y)
             (and
              (not (coordinate-in-circle? x y *circle*))
              (not (coordinate-in-region? x y *region*))))))
         (width-ratio (/ (window-width (current-window)) *arena-width*))
         (height-ratio (/ (window-height (current-window)) *arena-height*)))
    (draw-circle *circle* width-ratio height-ratio)
    (draw-region *region* width-ratio height-ratio)
    (draw-points yellow 5 points-in-region-and-circle width-ratio height-ratio)
    (draw-points red 5 points-not-in-region-nor-circle width-ratio height-ratio)
    (draw-points green 5 points-in-circle-not-region width-ratio height-ratio)
    (draw-points blue 5 points-in-region-not-circle width-ratio height-ratio)
    ))

;;; key-press.

(define (key-press key modifiers repeat?)
  (when (eq? key 'q)
    (abort-game))
  (match key
    ('d
     (set! *debug* (not *debug*)))
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
