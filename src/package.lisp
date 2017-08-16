;;;;
;;;;    This file is part of cl-pixman.
;;;;
;;;;    cl-pixman is free software: you can redistribute it and/or modify
;;;;    it under the terms of the GNU Lesser General Public License as
;;;;    published by the Free Software Foundation, either version 3 of the
;;;;    License, or (at your option) any later version.
;;;;
;;;;    cl-pixman is distributed in the hope that it will be useful,
;;;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;;    GNU Lesser General Public License for more details.
;;;;
;;;;    You should have received a copy of the GNU Lesser General Public License
;;;;    along with cl-pixman.  If not, see <http://www.gnu.org/licenses/>.
;;;;

(defpackage :pixman
  (:use #:cl #:cffi #:alexandria)
  (:export
   ;; extra-makes
   #:make-box
   #:make-color
   #:make-transform
   #:make-gradient-stop
   #:make-point-fixed
   ;; misc
   #:blt
   #:pixman-fill
   #:version
   #:version-string
   ;; transform
   #:transform-init-identity
   #:transform-point-3d
   #:transform-point
   #:transform-multiply
   #:transform-init-scale
   #:transform-scale
   #:transform-init-rotate
   #:transform-rotate
   #:transform-init-translate
   #:transform-translate
   #:transform-bounds
   #:transform-invert
   #:transform-identity-p
   #:transform-scale-p
   #:transform-int-translate-p
   #:transform-inverse-p
   ;; region
   #:make-region
   #:make-region-rect
   #:make-region-rects
   #:make-region-with-extents
   #:make-region-init-from-image
   #:region-fini
   ;; manipulation
   #:region-copy
   #:region-translate
   #:region-intersect
   #:region-union
   #:region-union-rect
   #:region-nunion-rect
   #:region-intersect-rect
   #:region-nintersect-rect
   #:region-subtract
   #:region-ninverse
   #:region-inverse
   #:region-empty-p
   #:region-rectangles
   #:region-equal
   #:region-print
   #:region-contains-point-p
   #:region-contains-rectangle
   #:region-extents
   #:region-num-rects
   #:region-selfcheck
   #:region-reset
   #:region-clear
   ;; image
   #:null-image
   #:image-create-solid-fill
   #:image-create-linear-gradient
   #:image-create-radial-gradient
   #:image-create-conical-gradient
   #:image-create-bits
   #:image-create-bits-no-clear
   #:image-unref
   #:format-supported-destionation
   #:format-supported-source
   ;; functional
   #:with-transform
   #:with-transforms
   #:make-identity
   #:make-translate
   #:make-scale
   #:make-rotate
   ;; properties
   #:image-set-clip-region
   #:image-set-has-client-clip
   #:image-set-transform
   #:image-set-repeat
   #:image-set-filter
   #:image-set-source-clipping
   #:image-set-alpha-map
   #:image-set-component-alpha
   #:image-get-component-alpha
   #:image-get-data
   #:image-get-width
   #:image-get-height
   #:image-get-stride
   #:image-get-depth
   #:image-get-format
   ;; composite
   #:compute-composite-region
   #:image-composite
   ;; types
   #:fixed-to-int
   #:int-to-fixed
   #:fixed-to-double
   #:double-to-fixed
   #:fixed-frac
   #:fixed-ceil
   #:fixed-fraction
   #:fixed-mod-2))
