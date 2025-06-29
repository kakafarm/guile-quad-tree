(use-modules
 (srfi srfi-1)
 (srfi srfi-9)
 (srfi srfi-11)

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

(define TAU (* 4 (acos 0)))

(define *width* 250)

(define *height* 250)

(define *use-quad-tree* #t)

(define *points*
  (let ((number-of-points 100))
    (unfold
     (lambda (seed) (>= seed number-of-points))
     (lambda (seed)
       (make-point (make-rectangular (random:uniform) (random:uniform))
                   (make-polar 0.01 (* TAU (random:uniform)))))
     1+
     0)))

(define *quad-tree*
  (quad-tree-insert-list
   (make-quad-tree
    (make-region #:x-low 0
                 #:x-high 1
                 #:y-low 0
                 #:y-high 1))
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
         (make-quad-tree (make-region #:x-low 0
                                      #:x-high 1
                                      #:y-low 0
                                      #:y-high 1))
         *points*
         (lambda (point)
           (values (real-part (point-position point))
                   (imag-part (point-position point)))))))

(define (update dt)
  (format #t "~A ~A~%" dt (elapsed-time))

  (for-each (lambda (point)
              (step-bump-point point)
              '(format #t
                      "~A ~A~%"
                      (real-part (point-position point))
                      (imag-part (point-position point))))
            *points*)

  (update-quad-tree)
  )

;;; draw.

(define (draw alpha)
  (let ((width (window-width (current-window)))
        (height (window-height (current-window))))
    (let ((result-items (quad-tree-locate-region *quad-tree*
                                                 (make-region #:x-low (/ 3 5)
                                                              #:x-high (/ 4 5)
                                                              #:y-low (/ 1 3)
                                                              #:y-high (/ 2 3))))
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
    (abort-game)))

;;; run-game.

(run-game
 #:window-width *width*
 #:window-height *height*
 #:update update
 #:draw draw
 #:key-press key-press)
