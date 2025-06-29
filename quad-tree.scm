;;; Guile quad-tree module.
;;; Copyright (C) 2017-2025  Drew Dudash.
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Notes:

;; Coordinates with a positive X are to the East of the origin.
;;
;; Coordinates with a negative X are to the West of the origin.
;;
;; Coordinates with a positive Y are to the North of the origin.
;;
;; Coordinates with a negative Y are to the South of the origin.
;;
;; x > 0: East.
;;
;; x < 0: West.
;;
;; y > 0: North.
;;
;; y < 0: South.

;;; Code:

(define-module (quad-tree)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-11)
  #:export (
            make-region
            region-x-low
            region-x-high
            region-y-low
            region-y-high

            make-quad-tree

            quad-tree-insert
            quad-tree-insert-list

            quad-tree-remove

            quad-tree-locate-position
            quad-tree-locate-area
            quad-tree-locate-region

            quad-tree-boundaries
            ))

(cond-expand
 (hoot
  ;; XXX: At the time of writing this code, Hoot does not support:
  ;;
  ;; (@ (srfi srfi-1) delete).
  (define* (delete x lst #:optional (= equal?))
    (reverse
     (fold
      (lambda (lst-element accumulator)
        (if (= x lst-element)
            accumulator
            (cons lst-element accumulator)))
      '()
      lst))))
 (else
  ))

;; Point boxes contain values and can be used in the quad tree.
;; TODO use vector from sly instead of custom vector
(define-record-type <point-box>
  (make-point-box x y value)
  point-box?
  (x point-box-x)
  (y point-box-y)
  (value point-box-value))

(define-record-type <region>
  (%make-region x-low x-high y-low y-high)
  region?
  (x-low region-x-low)
  (x-high region-x-high)
  (y-low region-y-low)
  (y-high region-y-high))

(define* (make-region #:key x-low x-high y-low y-high)
  (when (not (and x-low x-high y-low y-high))
    (error "All arguments must be provided." x-low x-high y-low y-high))
  (%make-region x-low x-high y-low y-high))

(define (point-box-position-equal? point-box-a point-box-b)
  (match-let ((($ <point-box> a-x a-y _)
               point-box-a)
              (($ <point-box> b-x b-y _)
               point-box-b))
    (and (= a-x b-x)
         (= a-y b-y))))

(define (region-includes-coordinate? region x y)
  (and (<= (region-x-low region)
           x
           (region-x-high region))
       (<= (region-y-low region)
           y
           (region-y-high region))))

(define (region-intersects-region? region-a region-b)
  (not (or (< (region-x-high region-a)
              (region-x-low region-b))
           (< (region-x-high region-b)
              (region-x-low region-a))
           (< (region-y-high region-a)
              (region-y-low region-b))
           (< (region-y-high region-b)
              (region-y-low region-a)))))

(define (region-quadrant region x y)
  "Find on which quadrant of @var{region} the coordinate given by @var{x} and @var{y} falls.

The coordinate (@var{x}, @var{y}) must be contained within
@var{region}, otherwise an error is raised.

On a Cartesian plane, with the center of @var{region} placed on the
Cartesian plane's origin:

Return @code{origin} if (@var{x}, @var{y}) is on the Cartesian origin.

Return @code{ne} if (@var{x}, @var{y}) is within @var{region} and
within quadrant I of the Cartesian plane, or the segment of X axis
just below quadrant I.

Return @code{nw} if (@var{x}, @var{y}) is within @var{region} and
within quadrant II of the Cartesian plane, or the segment of Y axis
just to the right of quadrant II.

Return @code{sw} if (@var{x}, @var{y}) is within @var{region} and
within quadrant III of the Cartesian plane, or the segment of X axis
just above quadrant III.

Return @code{se} if (@var{x}, @var{y}) is within @var{region} and
within quadrant IV of the Cartesian plane, or the segment of Y axis
just to the left quadrant IV."
  (let* ((x-low (region-x-low region))
         (x-high (region-x-high region))
         (y-low (region-y-low region))
         (y-high (region-y-high region))
         (x-center (/ (+ x-low x-high) 2))
         (y-center (/ (+ y-low y-high) 2)))
    (cond ((and (< x-center x)
                (<= x x-high)
                (<= y-center y)
                (<= y y-high))
           'ne)
          ((and (<= x-low x)
                (<= x x-center)
                (< y-center y)
                (<= y y-high))
           'nw)
          ((and (<= x-low x)
                (< x x-center)
                (<= y-low y)
                (<= y y-center))
           'sw)
          ((and (<= x-center x)
                (<= x x-high)
                (<= y-low y)
                (< y y-center))
           'se)
          ((and (= x x-center)
                (= y y-center))
           'origin)
          (else
           (error (format #f "point-box-x (~S, ~S) and point-box-y (~S, ~S) must be contained within REGION: x in [~S, ~S] y in [~S, ~S]"
                          x
                          (exact->inexact x)
                          y
                          (exact->inexact y)
                          (region-x-low region)
                          (region-x-high region)
                          (region-y-low region)
                          (region-y-high region))
                  )))))

(define (region-north-east region)
  (make-region #:x-low (/ (+ (region-x-low region)
                             (region-x-high region))
                          2)
               #:x-high (region-x-high region)
               #:y-low (/ (+ (region-y-low region)
                             (region-y-high region))
                          2)
               #:y-high (region-y-high region)))

(define (region-north-west region)
  (make-region #:x-low (region-x-low region)
               #:x-high (/ (+ (region-x-low region)
                              (region-x-high region))
                           2)
               #:y-low (/ (+ (region-y-low region)
                             (region-y-high region))
                          2)
               #:y-high (region-y-high region)))

(define (region-south-east region)
  (make-region #:x-low (/ (+ (region-x-low region)
                             (region-x-high region))
                          2)
               #:x-high (region-x-high region)
               #:y-low (region-y-low region)
               #:y-high (/ (+ (region-y-low region)
                              (region-y-high region))
                           2)))

(define (region-south-west region)
  (make-region #:x-low (region-x-low region)
               #:x-high (/ (+ (region-x-low region)
                              (region-x-high region))
                           2)
               #:y-low (region-y-low region)
               #:y-high (/ (+ (region-y-low region)
                              (region-y-high region))
                           2)))


;; The quad tree object is a wrapper over the recursively defined
;; node chain. The quad tree object contains several pieces of
;; meta data about the tree. The amount of items that can be stored on
;; a leaf node. The bounds of the quad-tree.
(define-record-type <quad-tree>
  (%make-quad-tree root bounds bucket-size)
  quad-tree?
  (root quad-tree-root)
  (bounds quad-tree-bounds)
  (bucket-size quad-tree-bucket-size))

;; A node that represents a four way split in the tree.
;; Contains four sub nodes and no values.
(define-record-type <branch-node>
  (make-branch-node north-east north-west south-west south-east)
  branch-node?
  (north-east branch-node-north-east)
  (north-west branch-node-north-west)
  (south-west branch-node-south-west)
  (south-east branch-node-south-east))

;; A leaf node. Contains zero or more values. The
;; zero value leaf node is the null node.
(define-record-type <leaf-node>
  (make-leaf-node items)
  leaf-node?
  (items leaf-node-items))

(define* (make-quad-tree region #:optional (bucket-size 1))
  (%make-quad-tree (make-leaf-node '())
                   region
                   bucket-size))

(define (quad-tree-insert tree x y value)
  "Return @var{tree} with @{value} added.

The original tree will not be modified.

Destructive operations on the new tree may affect the old tree."
  (let ((old-root (quad-tree-root tree))
        (bucket-size (quad-tree-bucket-size tree))
        (bounds (quad-tree-bounds tree)))
    (unless (region-includes-coordinate? bounds x y)
      (error (format #f
                     "The point (~a, ~a) is not inside the region ~a of this tree."
                     x y bounds)))
    (%make-quad-tree
     (insert-helper old-root
                    bucket-size
                    bounds
                    (make-point-box x y value))
     bounds
     bucket-size)))

(define (quad-tree-locate-position tree x y)
  "Find the value stored in @var{tree} at the coordinate given by @var{x} and @var{y}."
  (locate-position-helper (quad-tree-root tree)
                          (quad-tree-bucket-size tree)
                          (quad-tree-bounds tree)
                          x
                          y
                          ))

(define (quad-tree-locate-area tree quadrant-in-area? coordinate-in-area?)
  "Find all values in the given TREE and the given BOUNDS."
  (locate-area-helper #:node (quad-tree-root tree)
                      #:region (quad-tree-bounds tree)
                      #:quadrant-in-area? quadrant-in-area?
                      #:coordinate-in-area? coordinate-in-area?))

(define (quad-tree-locate-circle tree x y radius)
  (quad-tree-locate-area tree
                         (make-quadrant-intersects-circle? x y radius)
                         (lambda (x y)
                           (point-in-circle? x y (* radius radius)))))

(define (quad-tree-locate-region tree region)
  (quad-tree-locate-area tree
                         (lambda (quadrant)
                           (region-intersects-region? region quadrant))
                         (lambda (quadrant item)
                           (region-includes-coordinate?
                            region
                            (point-box-x item)
                            (point-box-y item)))))

(define (quad-tree-remove tree val)
  "Return TREE with VAL removed. The original tree will not be modified. Destructive operations on the new tree may affect the old tree."
  (let ((old-root (quad-tree-root tree))
        (bucket-size (quad-tree-bucket-size tree))
        (bounds (quad-tree-bounds tree)))
    (%make-quad-tree
     (remove-helper old-root
                    (quad-tree-bounds tree)
                    bucket-size
                    bounds
                    val)
     bounds
     bucket-size)))

(define (quad-tree-insert-list quad-tree items item-xy)
  "Return a @var{quad-tree} with the individual elements of the list
@var{items} inserted to it.

@var{item-xy} is a procedure whose input is a single value of the type
or types of the individual elements of the list @var{items}.  The
output of @var{item-xy} is a coordinate in the form of two values -
@var{(values x y)}."
  (let loop ((new-quad-tree quad-tree)
             (items items))
    (match items
      ('() new-quad-tree)
      ((item . rest-of-items)
       (let-values (((x y) (item-xy item)))
         (loop (quad-tree-insert new-quad-tree
                                 x
                                 y
                                 item)
               rest-of-items))))))

(define (list-replace-or-length new-item item-list =?)
  "Return the @var{item-list}, whether it was changed, and its length.

If there is an item in @var{item-list} that is equal to @var{new-item} according to the equality procedure @var{=?}, return:

- @var{item-list} with only the first item in @var{item-list} from the
left replaced with @var{new-item}.

- The value #t representing that the item was replaced.

- The value #f because we do not care about length.

If there is no item in @var{item-list} equal to @var{new-item}
according to @var{=?}, return:

- @var{item-list} with its elements unchanged.

- The value #f representing that there was no change in the values of
the returned list in comparison to the input @var{item-list}.

- The length of @var{item-list}."
  (let loop ((item-list item-list)
             (replaced #f)
             (count 0))
    (match item-list
      ('()
       (values '() replaced count))
      ((old-item . rest-of-items)
       (cond
        ((=? old-item new-item)
         (values (cons new-item rest-of-items)
                 #t
                 #f))
        (else
         (let-values (((new-list replaced new-count)
                       (loop rest-of-items
                             replaced
                             (1+ count))))
           (values (cons old-item new-list)
                   replaced
                   new-count))))))))

(define (insert-helper node bucket-size region point-box)
  (match node
    (($ <branch-node> north-east north-west south-west south-east)
     (case (region-quadrant region (point-box-x point-box) (point-box-y point-box))
       ((ne origin)
        (make-branch-node (insert-helper north-east
                                         bucket-size
                                         (region-north-east region)
                                         point-box)
                          north-west
                          south-west
                          south-east))
       ((nw)
        (make-branch-node north-east
                          (insert-helper north-west
                                         bucket-size
                                         (region-north-west region)
                                         point-box)
                          south-west
                          south-east))
       ((sw)
        (make-branch-node north-east
                          north-west
                          (insert-helper south-west
                                         bucket-size
                                         (region-south-west region)
                                         point-box)
                          south-east))
       ((se)
        (make-branch-node north-east
                          north-west
                          south-west
                          (insert-helper south-east
                                         bucket-size
                                         (region-south-east region)
                                         point-box)))))
    (($ <leaf-node> items)
     (let-values (((items replaced items-length)
                   ;; FIXME: Add an argument asking whether or not
                   ;; replace existing items on same coordinate.
                   (list-replace-or-length point-box
                                           items
                                           point-box-position-equal?
                                           )))
       (cond
        (replaced
         (make-leaf-node items))
        ((< items-length bucket-size)
         (make-leaf-node (cons point-box items)))
        (else
         (fold (lambda (point-box node)
                 (insert-helper node
                                bucket-size
                                region
                                point-box))
               (make-branch-node (make-leaf-node (list))
                                 (make-leaf-node (list))
                                 (make-leaf-node (list))
                                 (make-leaf-node (list)))
               (cons point-box items))))))))

(define (remove-helper node region bucket-size point-box val)
  (match node
    (($ <branch-node> north-east north-west south-west south-east)
     (case (region-quadrant region (point-box-x point-box) (point-box-y point-box))
       ((ne origin)
        (make-branch-node (remove-helper north-east
                                         (region-north-east region)
                                         bucket-size
                                         point-box
                                         val)
                          north-west
                          south-west
                          south-east))
       ((nw)
        (make-branch-node north-east
                          (remove-helper north-west
                                         (region-north-west region)
                                         bucket-size
                                         point-box
                                         val)
                          south-west
                          south-east))
       ((sw)
        (make-branch-node north-east
                          north-west
                          (remove-helper (branch-node-south-west node)
                                         (region-south-west region)
                                         bucket-size
                                         point-box
                                         val)
                          south-east))
       ((se)
        (make-branch-node north-east
                          north-west
                          south-west
                          (remove-helper south-east
                                         (region-south-east region)
                                         bucket-size
                                         point-box
                                         val)))))
    (($ <leaf-node> items)
     (make-leaf-node (delete val (leaf-node-items node))))))

(define (locate-position-helper node bucket-size region x y)
  (match node
    (($ <branch-node> north-east north-west south-west south-east)
     (case (region-quadrant region x y)
       ((ne origin)
        (locate-position-helper north-east
                                bucket-size
                                (region-north-east region)
                                x
                                y))
       ((nw)
        (locate-position-helper north-west
                                bucket-size
                                (region-north-west region)
                                x
                                y))
       ((sw)
        (locate-position-helper south-west
                                bucket-size
                                (region-south-west region)
                                x
                                y))
       ((se)
        (locate-position-helper south-east
                                bucket-size
                                (region-south-east region)
                                x
                                y))))
    (($ <leaf-node> items)
     (let loop ((items items))
       (match items
         ('() #f)
         ((item . rest-of-items)
          (if (and (= x (point-box-x item))
                   (= y (point-box-y item)))
              (point-box-value item)
              (loop rest-of-items))))))))

(define (point-in-circle? x y radius-squared)
  "Returns #t if the coordinate given by @var{x} and @var{y} is in a
circle whose center is on the origin and its radius is defined as the
square root of @var{radius-squared}, otherwise #f."
  (<= (+ (* x x)
         (* y y))
      radius-squared))

(define (make-quadrant-intersects-circle? circle-x circle-y radius)
  (when (< radius 0) (error "Radius must not be negative:" radius))
  (let ((radius-squared (* radius radius)))
    (define (clamp value minimum maximum)
      (cond
       ((< value minimum) minimum)
       ((< maximum value) maximum)
       (else value)))
    (lambda (quadrant)
      (let* ((closest-edge-to-circle-x (clamp circle-x
                                              (region-x-low quadrant)
                                              (region-x-high quadrant)))
             (closest-edge-to-circle-y (clamp circle-y
                                              (region-y-low quadrant)
                                              (region-y-high quadrant))))
        (point-in-circle? (- circle-x closest-edge-to-circle-x)
                          (- circle-y closest-edge-to-circle-y)
                          radius-squared)))))

(define* (locate-area-helper #:key node region quadrant-in-area? coordinate-in-area?)
  (match node
    (($ <branch-node> north-east north-west south-west south-east)
     (apply append
            (map (lambda (get-quadrant branch)
                   (let ((quadrant (get-quadrant region)))
                     (cond
                      ((quadrant-in-area? quadrant)
                       (locate-area-helper #:node branch
                                           #:region quadrant
                                           #:quadrant-in-area? quadrant-in-area?
                                           #:coordinate-in-area? coordinate-in-area?))
                      (else '()))))
                 (list region-north-east
                       region-north-west
                       region-south-east
                       region-south-west)
                 (list north-east
                       north-west
                       south-west
                       south-east))))
    (($ <leaf-node> items)
     (filter (lambda (item) (coordinate-in-area? region item)) items))))

(define (quad-tree-boundaries quad-tree)
  (let loop ((current-boundary (quad-tree-bounds quad-tree))
             (current-node (quad-tree-root quad-tree)))
    (match current-node
      (($ <branch-node> north-east north-west south-west south-east)
       (append
        (list current-boundary)
        (loop (region-north-east current-boundary) north-east)
        (loop (region-north-west current-boundary) north-west)
        (loop (region-south-west current-boundary) south-west)
        (loop (region-south-east current-boundary) south-east)
        ))
      (($ <leaf-node> items)
       (list current-boundary)))))
