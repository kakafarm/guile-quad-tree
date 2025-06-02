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
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-1)
  #:export (
            make-region
            make-quad-tree
            quad-tree-insert
            quad-tree-remove
            quad-tree-locate-position
            quad-tree-locate-area
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

(define (region-includes? region x y)
  (and (<= (region-x-low region)
           x
           (region-x-high region))
       (<= (region-y-low region)
           y
           (region-y-high region))))

(define (region-quadrant region point-box)
  "Find on which quadrant of a @var{region} a @var{point-box} falls on.

@var{point-box} must be contained within @var{region}, otherwise an error is raised.

On a Cartesian plane, with the center of @var{region} placed on the Cartesian plane's origin:

Return @code{origin} if @var{point-box} is on the Cartesian origin.

Return @code{ne} if @var{point-box} is within @var{region} and within quadrant I of the Cartesian plane, or the segment of X axis just below quadrant I.

Return @code{nw} if @var{point-box} is within @var{region} and within quadrant II of the Cartesian plane, or the segment of Y axis just to the right of quadrant II.

Return @code{sw} if @var{point-box} is within @var{region} and within quadrant III of the Cartesian plane, or the segment of X axis just above quadrant III.

Return @code{se} if @var{point-box} is within @var{region} and within quadrant IV of the Cartesian plane, or the segment of Y axis just to the left quadrant IV."
  (let* ((x (point-box-x point-box))
         (y (point-box-y point-box))
         (x-low (region-x-low region))
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
           (error "X and Y must be contained within REGION:" x y region)))))

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
  (make-branch-node ne nw se sw)
  branch-node?
  (ne branch-node-ne)
  (nw branch-node-nw)
  (se branch-node-se)
  (sw branch-node-sw))

;; A leaf node. Contains zero or more values. The
;; zero value leaf node is the null node.
(define-record-type <leaf-node>
  (make-leaf-node items)
  leaf-node?
  (items leaf-node-items))

(define* (make-quad-tree region #:optional bucket-size)
  (let ((bucket-size (if bucket-size bucket-size 1)))
    (%make-quad-tree (make-leaf-node '()) region  bucket-size)))

(define (quad-tree-insert tree x y value)
  "Return TREE with VAL added. The original tree will not be modified. Destructive operations on the new tree may affect the old tree."
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
     bucket-size
     bounds)))

(define (quad-tree-locate-position tree x y)
  "Find the value in the TREE at the given LOCATION."
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
     bucket-size
     bounds)))

(define (insert-helper node bucket-size region point-box)
  (cond ((branch-node? node)
         (case (region-quadrant region point-box)
           ((ne origin)
            (make-branch-node (insert-helper (branch-node-ne node)
                                             bucket-size
                                             region
                                             point-box)
                              (branch-node-nw node)
                              (branch-node-sw node)
                              (branch-node-se node)))
           ((nw)
            (make-branch-node (branch-node-ne node)
                              (insert-helper (branch-node-nw node)
                                             bucket-size
                                             region
                                             point-box)
                              (branch-node-sw node)
                              (branch-node-se node)))
           ((sw)
            (make-branch-node (branch-node-ne node)
                              (branch-node-nw node)
                              (insert-helper (branch-node-sw node)
                                             bucket-size
                                             region
                                             point-box)
                              (branch-node-se node)))
           ((se)
            (make-branch-node (branch-node-ne node)
                              (branch-node-nw node)
                              (branch-node-sw node)
                              (insert-helper (branch-node-se node)
                                             bucket-size
                                             region
                                             point-box)))))
        ((leaf-node? node)
         (let ((items (leaf-node-items node)))
           (if (< (length items) bucket-size)
               (make-leaf-node (cons point-box items))
               (fold (lambda (node item)
                       (insert-helper node
                                      bucket-size
                                      region
                                      point-box))
                     (make-branch-node (make-leaf-node (list))
                                       (make-leaf-node (list))
                                       (make-leaf-node (list))
                                       (make-leaf-node (list)))
                     (cons point-box items)))))))

(define (remove-helper node bucket-size point-box val)
  (cond ((branch-node? node)
         (case (region-quadrant region point-box)
           ((ne origin)
            (make-branch-node (remove-helper (branch-node-ne node)
                                             bucket-size
                                             point-box
                                             val)
                              (branch-node-nw node)
                              (branch-node-sw node)
                              (branch-node-se node)))
           ((nw)
            (make-branch-node (branch-node-ne node)
                              (remove-helper (branch-node-nw node)
                                             bucket-size
                                             point-box
                                             val)
                              (branch-node-sw node)
                              (branch-node-se node)))
           ((sw)
            (make-branch-node (branch-node-ne node)
                              (branch-node-nw node)
                              (remove-helper (branch-node-sw node)
                                             bucket-size
                                             point-box
                                             val)
                              (branch-node-se node)))
           ((se)
            (make-branch-node (branch-node-ne node)
                              (branch-node-nw node)
                              (branch-node-sw node)
                              (remove-helper (branch-node-se node)
                                             bucket-size
                                             point-box
                                             val)))))
        ((leaf-node? node)
         (make-leaf-node (delete val (leaf-node-items node))))))

(define (locate-position-helper node bucket-size region x y)
  (cond ((branch-node? node)
         (case (region-quadrant region point-box)
           ((ne origin)
            (locate-position-helper (branch-node-ne node)
                                    bucket-size
                                    (region-north-east region)
                                    x
                                    y))
           ((nw)
            (locate-position-helper (branch-node-nw node)
                                    bucket-size
                                    (region-north-west region)
                                    x
                                    y))
           ((sw)
            (locate-position-helper (branch-node-sw node)
                                    bucket-size
                                    (region-south-west region)
                                    x
                                    y))
           ((se)
            (locate-position-helper (branch-node-se node)
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
