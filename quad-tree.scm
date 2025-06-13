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

(define-module (quad-tree)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-11)
  #:export (
            make-region
            make-quad-tree
            quad-tree-insert
            quad-tree-remove
            quad-tree-locate-position
            quad-tree-locate-area
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
  (make-region x-low x-high y-low y-high)
  region?
  (x-low region-x-low)
  (x-high region-x-high)
  (y-low region-y-low)
  (y-high region-y-high))

(define (point-box-position-equal? point-box-a point-box-b)
  (match-let ((($ <point-box> a-x a-y _)
               point-box-a)
              (($ <point-box> b-x b-y _)
               point-box-b))
    (and (= a-x b-x)
         (= a-y b-y))))

(define (region-includes? region x y)
  (and (<= (region-x-low region)
           x
           (region-x-high region))
       (<= (region-y-low region)
           y
           (region-y-high region))))

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
  (make-region (/ (+ (region-x-low region)
                     (region-x-high region))
                  2)
               (region-x-high region)
               (/ (+ (region-y-low region)
                     (region-y-high region))
                  2)
               (region-y-high region)))

(define (region-north-west region)
  (make-region (region-x-low region)
               (/ (+ (region-x-low region)
                     (region-x-high region))
                  2)
               (/ (+ (region-y-low region)
                     (region-y-high region))
                  2)
               (region-y-high region)))

(define (region-south-east region)
  (make-region (/ (+ (region-x-low region)
                     (region-x-high region))
                  2)
               (region-x-high region)
               (region-y-low region)
               (/ (+ (region-y-low region)
                     (region-y-high region))
                  2)))

(define (region-south-west region)
  (make-region (region-x-low region)
               (/ (+ (region-x-low region)
                     (region-x-high region))
                  2)
               (region-y-low region)
               (/ (+ (region-y-low region)
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

(define* (make-quad-tree region #:optional bucket-size)
  (let ((bucket-size (if bucket-size bucket-size 1)))
    (%make-quad-tree (make-leaf-node '()) region bucket-size)))

(define (quad-tree-insert tree x y value)
  "Return @var{tree} with @{value} added.

The original tree will not be modified.

Destructive operations on the new tree may affect the old tree."
  (let ((old-root (quad-tree-root tree))
        (bucket-size (quad-tree-bucket-size tree))
        (bounds (quad-tree-bounds tree)))
    (unless (region-includes? bounds x y)
      (error (format #f
                     "The point ~a is not inside the region ~a of this tree."
                     point-box bounds)))
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

(define (quad-tree-locate-area tree bounds)
  "Find all values in the given TREE and the given BOUNDS."
  (locate-area-helper (quad-tree-root tree)))

(define (quad-tree-remove tree val)
  "Return TREE with VAL removed. The original tree will not be modified. Destructive operations on the new tree may affect the old tree."
  (let ((old-root (quad-tree-root tree))
        (bucket-size (quad-tree-bucket-size tree))
        (bounds (quad-tree-bounds tree)))
    (%make-quad-tree
     (remove-helper old-root
                    bucket-size
                    bounds
                    val)
     bounds
     bucket-size)))

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
  (cond ((branch-node? node)
         (case (region-quadrant region (point-box-x point-box) (point-box-y point-box))
           ((ne origin)
            (make-branch-node (insert-helper (branch-node-north-east node)
                                             bucket-size
                                             (region-north-east region)
                                             point-box)
                              (branch-node-north-west node)
                              (branch-node-south-west node)
                              (branch-node-south-east node)))
           ((nw)
            (make-branch-node (branch-node-north-east node)
                              (insert-helper (branch-node-north-west node)
                                             bucket-size
                                             (region-north-west region)
                                             point-box)
                              (branch-node-south-west node)
                              (branch-node-south-east node)))
           ((sw)
            (make-branch-node (branch-node-north-east node)
                              (branch-node-north-west node)
                              (insert-helper (branch-node-south-west node)
                                             bucket-size
                                             (region-south-west region)
                                             point-box)
                              (branch-node-south-east node)))
           ((se)
            (make-branch-node (branch-node-north-east node)
                              (branch-node-north-west node)
                              (branch-node-south-west node)
                              (insert-helper (branch-node-south-east node)
                                             bucket-size
                                             (region-south-east region)
                                             point-box)))))
        ((leaf-node? node)
         (let-values (((items replaced items-length)
                       (list-replace-or-length point-box
                                               (leaf-node-items node)
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

(define (remove-helper node bucket-size point-box val)
  (cond ((branch-node? node)
         (case (region-quadrant region point-box)
           ((ne origin)
            (make-branch-node (remove-helper (branch-node-north-east node)
                                             bucket-size
                                             point-box
                                             val)
                              (branch-node-north-west node)
                              (branch-node-south-west node)
                              (branch-node-south-east node)))
           ((nw)
            (make-branch-node (branch-node-north-east node)
                              (remove-helper (branch-node-north-west node)
                                             bucket-size
                                             point-box
                                             val)
                              (branch-node-south-west node)
                              (branch-node-south-east node)))
           ((sw)
            (make-branch-node (branch-node-north-east node)
                              (branch-node-north-west node)
                              (remove-helper (branch-node-south-west node)
                                             bucket-size
                                             point-box
                                             val)
                              (branch-node-south-east node)))
           ((se)
            (make-branch-node (branch-node-north-east node)
                              (branch-node-north-west node)
                              (branch-node-south-west node)
                              (remove-helper (branch-node-south-east node)
                                             bucket-size
                                             point-box
                                             val)))))
        ((leaf-node? node)
         (make-leaf-node (delete val (leaf-node-items node))))))

(define (locate-position-helper node bucket-size region x y)
  (cond ((branch-node? node)
         (case (region-quadrant region x y)
           ((ne origin)
            (locate-position-helper (branch-node-north-east node)
                                    bucket-size
                                    (region-north-east region)
                                    x
                                    y))
           ((nw)
            (locate-position-helper (branch-node-north-west node)
                                    bucket-size
                                    (region-north-west region)
                                    x
                                    y))
           ((sw)
            (locate-position-helper (branch-node-south-west node)
                                    bucket-size
                                    (region-south-west region)
                                    x
                                    y))
           ((se)
            (locate-position-helper (branch-node-south-east node)
                                    bucket-size
                                    (region-south-east region)
                                    x
                                    y))))
        ((leaf-node? node)
         (let loop ((items (leaf-node-items node)))
           (match items
             ('() #f)
             ((item . rest-of-items)
              (if (and (= x (point-box-x item))
                       (= y (point-box-y item)))
                  (point-box-value item)
                  (loop rest-of-items))))))))

(define (locate-area-helper node bucket-size bounds)
  (cond ((branch-node? node)
         #:f)
        ((leaf-node? node)
         #:f)))
