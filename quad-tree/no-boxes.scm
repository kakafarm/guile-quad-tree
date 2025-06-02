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

(define-module (quad-tree no-boxes)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
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

(define (region-quadrant region x y)
  "Find on which quadrant of a @var{region} a @var{x y value} falls on.

@var{x y value} must be contained within @var{region}, otherwise an error is raised.

On a Cartesian plane, with the center of @var{region} placed on the Cartesian plane's origin:

Return @code{origin} if @var{x y value} is on the Cartesian origin.

Return @code{ne} if @var{x y value} is within @var{region} and within quadrant I of the Cartesian plane, or the segment of X axis just below quadrant I.

Return @code{nw} if @var{x y value} is within @var{region} and within quadrant II of the Cartesian plane, or the segment of Y axis just to the right of quadrant II.

Return @code{sw} if @var{x y value} is within @var{region} and within quadrant III of the Cartesian plane, or the segment of X axis just above quadrant III.

Return @code{se} if @var{x y value} is within @var{region} and within quadrant IV of the Cartesian plane, or the segment of Y axis just to the left quadrant IV."
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

;; A leaf node. Contains zero or more values. The
;; zero value leaf node is the null node.
(define-record-type <leaf-node>
  (make-leaf-node items)
  leaf-node?
  (items leaf-node-items))

(define make-quad-tree
  (case-lambda
    ((region bucket-size)
     (%make-quad-tree (make-leaf-node '()) region  bucket-size))
    ((region)
     (%make-quad-tree (make-leaf-node '()) region 1))))

(define (quad-tree-insert tree item item->xy)
  "Return TREE with VAL added. The original tree will not be modified. Destructive operations on the new tree may affect the old tree."
  (let ((old-root (quad-tree-root tree))
	(bucket-size (quad-tree-bucket-size tree))
	(bounds (quad-tree-bounds tree)))
    (let-values (((x y) (item->xy item)))
      (unless (region-includes? bounds x y)
        (error (format #f
                       "The point x=~a y=~a is not inside the region ~a of this tree."
                       x
                       y
                       bounds))))
    (%make-quad-tree
     (insert-helper old-root
		    bucket-size
		    bounds
                    item
                    item->xy)
     bucket-size
     bounds)))

(define* (quad-tree-locate-position tree x y item->xy)
  "Find the value in the @var{tree} at the given (@var{x}, @{y}) location."
  (locate-position-helper (quad-tree-root tree)
			  (quad-tree-bucket-size tree)
			  (quad-tree-bounds tree)
                          x
                          y
                          item->xy
                          ))

(define (quad-tree-locate-area tree intersects?)
  "Find all values in the given TREE and the given BOUNDS."
  (locate-area-helper (quad-tree-root tree) intersects?))

(define (quad-tree-remove tree x y value)
  "Return TREE with VAL removed. The original tree will not be modified. Destructive operations on the new tree may affect the old tree."
  (let ((old-root (quad-tree-root tree))
	(bucket-size (quad-tree-bucket-size tree))
	(bounds (quad-tree-bounds tree)))
    (%make-quad-tree
     (remove-helper old-root
		    bucket-size
		    bounds
                    x
                    y
                    value)
     bucket-size
     bounds)))

;; A node that represents a four way split in the tree.
;; Contains four sub nodes and no values.
(define-record-type <branch-node>
  (make-branch-node ne nw se sw)
  branch-node?
  (ne branch-node-ne)
  (nw branch-node-nw)
  (se branch-node-se)
  (sw branch-node-sw))

(define (insert-helper node bucket-size region item item->xy)
  (cond ((branch-node? node)
         (let-values (((x y) (item->xy item)))
           (case (region-quadrant region x y)
             ((ne origin)
              (make-branch-node (insert-helper (branch-node-ne node)
                                               bucket-size
                                               region
                                               item
                                               item->xy)
                                (branch-node-nw node)
                                (branch-node-sw node)
                                (branch-node-se node)))
             ((nw)
              (make-branch-node (branch-node-ne node)
                                (insert-helper (branch-node-nw node)
                                               bucket-size
                                               region
                                               item
                                               item->xy)
                                (branch-node-sw node)
                                (branch-node-se node)))
             ((sw)
              (make-branch-node (branch-node-ne node)
                                (branch-node-nw node)
                                (insert-helper (branch-node-sw node)
                                               bucket-size
                                               region
                                               item
                                               item->xy)
                                (branch-node-se node)))
             ((se)
              (make-branch-node (branch-node-ne node)
                                (branch-node-nw node)
                                (branch-node-sw node)
                                (insert-helper (branch-node-se node)
                                               bucket-size
                                               region
                                               item
                                               item->xy))))))
	((leaf-node? node)
	 (let ((items (leaf-node-items node)))
	   (if (< (length items) bucket-size)
               (make-leaf-node (cons item items))
               (fold (lambda (node item)
		       (insert-helper node bucket-size region item item->xy))
		     (make-branch-node (make-leaf-node (list))
				       (make-leaf-node (list))
				       (make-leaf-node (list))
				       (make-leaf-node (list)))
		     (cons item items)))))))

(define (remove-helper node bucket-size bounds x y value)
  (cond ((branch-node? node)
	 (case (region-quadrant bounds x y)
	   ((ne origin)
	    (make-branch-node (remove-helper (branch-node-ne node)
					     bucket-size
                                             bounds
					     x
                                             y
                                             value)
			      (branch-node-nw node)
			      (branch-node-sw node)
			      (branch-node-se node)))
	   ((nw)
	    (make-branch-node (branch-node-ne node)
			      (remove-helper (branch-node-nw node)
					     bucket-size
					     bounds
                                             x
                                             y
                                             value)
			      (branch-node-sw node)
			      (branch-node-se node)))
	   ((sw)
	    (make-branch-node (branch-node-ne node)
			      (branch-node-nw node)
			      (remove-helper (branch-node-sw node)
					     bucket-size
					     bounds
                                             x
                                             y
                                             value)
			      (branch-node-se node)))
	   ((se)
	    (make-branch-node (branch-node-ne node)
			      (branch-node-nw node)
			      (branch-node-sw node)
			      (remove-helper (branch-node-se node)
					     bucket-size
					     bounds
                                             x
                                             y
                                             value)))))
	((leaf-node? node)
	 (make-leaf-node (delete value (leaf-node-items node))))))

(define (locate-position-helper node bucket-size region query-x query-y item->xy)
  (cond ((branch-node? node)
	 (case (region-quadrant region query-x query-y)
	   ((ne origin)
	    (make-branch-node (locate-position-helper (branch-node-ne node)
                                                      bucket-size
                                                      region
                                                      query-x
                                                      query-y)
			      (branch-node-nw node)
			      (branch-node-sw node)
			      (branch-node-se node)))
	   ((nw)
	    (make-branch-node (branch-node-ne node) (locate-position-helper
			                             (branch-node-nw node)
			                             bucket-size
                                                     region
			                             query-x
                                                     query-y)
			      (branch-node-sw node)
			      (branch-node-se node)))
	   ((sw)
	    (make-branch-node (branch-node-ne node)
			      (branch-node-nw node)
			      (locate-position-helper (branch-node-sw node)
			                                bucket-size
                                                        region
			                                query-x
                                                        query-y)
			      (branch-node-se node)))
	   ((se)
	    (make-branch-node (branch-node-ne node)
			      (branch-node-nw node)
			      (branch-node-sw node) (locate-position-helper
			                             (branch-node-se node)
			                             bucket-size
                                                     region
			                             query-x
                                                     query-y)))))
	((leaf-node? node)
         (let loop ()
           (match (leaf-node-items node)
             ('() #f)
             ((value . rest-of-items)
              (let-values (((item-x item-y) (item->xy value)))
                (if (and (= item-x query-x)
                         (= item-y query-y))
                    (list value)
                    #f))))))))

(define (locate-area-helper node bucket-size intersects?)
  (cond ((branch-node? node)
	 #:f)
	((leaf-node? node)
	 #:f)))
