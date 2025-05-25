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
  #:export (make-quad-tree
	    quad-tree-insert
	    quad-tree-remove
	    quad-tree-locate-position
	    quad-tree-locate-area))

;; Same position could lead to an infinite loop of subdivisions.
;; Check for given bounds, but use a power of 2 for actual bounds.
(define (log2 n)
  "Take the log of a number using base 2."
  (/ (log n) (log 2)))

;; Point boxes contain values and can be used in the quad tree.
;; TODO use vector from sly instead of custom vector
(define-record-type <point-box>
  (make-point-box position value)
  point-box?
  (position point-box-position)
  (value point-box-value))

(define-record-type <region>
  (make-region x-low x-high y-low y-high)
  region?
  (x-low region-x-low)
  (x-high region-x-high)
  (y-low region-y-low)
  (y-high region-y-high))

(define (region-includes? region vec)
  (and (<= (region-x-low region)
	   (vx vec)
	   (region-x-high region))
       (<= (region-y-low region)
	   (vy vec)
	   (region-y-high region))))

(define-record-type <vector2>
  (vector2 x y)
  vector2?
  (x vector2-x)
  (y vector2-y))

(define-record-type <vector3>
  (vector3 x y z)
  vector3?
  (x vector3-x)
  (y vector3-y)
  (z vector3-z))

(define-record-type <vector4>
  (vector4 x y z w)
  vector4?
  (x vector4-x)
  (y vector4-y)
  (z vector4-z)
  (w vector4-w))

(define vx
  (match-lambda
   ((or ($ <vector2> x _)
        ($ <vector3> x _ _)
        ($ <vector4> x _ _ _))
    x)))

(define vy
  (match-lambda
   ((or ($ <vector2> _ y)
        ($ <vector3> _ y _)
        ($ <vector4> _ y _ _))
    y)))

(define (region-quadrant region point-box)
  (let* ((x (vx (point-box-position point-box)))
	 (y (vy (point-box-position point-box)))
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
	   'origin))))

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
;; a leaf node. The bounds of the quad-tree. And the smallest bounds
;; with dimensions that are a power of 2 that contain the bounds of the quad-tree
;; A quad tree must have even bounds to subdivide properly. The real bounds are
;; saved to check inserted nodes to make sure they're in bounds.
(define-record-type <quad-tree>
  (%make-quad-tree root real-bound even-bound bucket-size)
  quad-tree?
  (root quad-tree-root)
  (real-bound quad-tree-real-bound)
  (even-bound quad-tree-even-bound)
  (bucket-size quad-tree-bucket-size))

(define (make-even-region real-region)
  "Takes a region and returns the smallest region that contains it, but has dimensions that are a power of 2."
  real-region)

(define make-quad-tree
  (case-lambda
    ((real-region bucket-size)
     (%make-quad-tree (list) real-region (make-even-region real-region)  bucket-size))
    ((real-region)
     (%make-quad-tree (list) real-region (make-even-region real-region) 1))))

(define (quad-tree-insert tree point-box)
  "Return TREE with VAL added. The original tree will not be modified. Destructive operations on the new tree may affect the old tree."
  (let ((old-root (quad-tree-root tree))
	(bucket-size (quad-tree-bucket-size tree))
	(even-bound (quad-tree-even-bound tree))
	(real-bound (quad-tree-real-bound tree)))
    (unless (region-includes? real-bound point-box)
      (error (format #f
		     "The point ~a is not inside the region ~a of this tree."
		     point-box real-bound)))
    (%make-quad-tree
     (remove-helper old-root
		    bucket-size
		    even-bound
		    point-box)
     bucket-size
     even-bound
     real-bound)))

(define (quad-tree-locate-position tree location)
  "Find the value in the TREE at the given LOCATION."
  (locate-position-helper (quad-tree-root tree)
			  (quad-tree-bucket-size tree)
			  (quad-tree-even-bound tree)))

(define (quad-tree-locate-area tree bound)
  "Find all values in the given TREE and the given BOUND."
  (locate-area-helper (quad-tree-root tree)))

(define (quad-tree-remove tree val)
    "Return TREE with VAL removed. The original tree will not be modified. Destructive operations on the new tree may affect the old tree."
  (let ((old-root (quad-tree-root tree))
	(bucket-size (quad-tree-bucket-size tree))
	(even-bound (quad-tree-even-bound tree))
	(real-bound (quad-tree-real-bound tree)))
    (%make-quad-tree
     (remove-helper old-root
		    bucket-size
		    even-bound
		    val)
     bucket-size
     even-bound
     real-bound)))

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

(define (insert-helper node bucket-size region point-box)
  (cond (((branch-node? node))
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
					     point-box))))
	 ((leaf-node? node)
	  (let ((items (leaf-node-items node)))
	    (if (> (1+ (length items)) bucket-size)
		(fold (lambda (node item)
			(insert-helper node bucket-size
				       region point-box))
		      (make-branch-node (make-leaf-node (list))
					(make-leaf-node (list))
					(make-leaf-node (list))
					(make-leaf-node (list)))
		      (cons point-box items))
		(make-leaf-node (cons point-box items))))))))

(define (remove-helper node bucket-size point-box val)
  (cond (((branch-node? node))
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
					     val))))
	 ((leaf-node? node)
	  (make-leaf-node (delete val (leaf-node-items node)))))))

(define (locate-position-helper node bucket-size region point-box)
  (cond (((branch-node? node))
	 (case (region-quadrant region point-box)
	   ((ne origin)
	    (make-branch-node (locate-position-helper
			       (branch-node-ne node)
			       bucket-size
			       position)
			      (branch-node-nw node)
			      (branch-node-sw node)
			      (branch-node-se node)))
	   ((nw)
	    (make-branch-node (branch-node-ne node)
			      (location-position-helper
			       (branch-node-nw node)
			       bucket-size
			       position)
			      (branch-node-sw node)
			      (branch-node-se node)))
	   ((sw)
	    (make-branch-node (branch-node-ne node)
			      (branch-node-nw node)
			      (location-position-helper
			       (branch-node-sw node)
			       bucket-size
			       position)
			      (branch-node-se node)))
	   ((se)
	    (make-branch-node (branch-node-ne node)
			      (branch-node-nw node)
			      (branch-node-sw node)
			      (location-position-helper
			       (branch-node-se node)
			       bucket-size
			       position))))
	 ((leaf-node? node)
	  #:f))))

(define (locate-area-helper node bucket-size bound)
  (cond (((branch-node? node)
	  #:f)
	 ((leaf-node? node)
	  #:f))))
