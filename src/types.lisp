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

(in-package :pixman)

(use-foreign-library pixman)

(defctype image :pointer)

(defctype fixed-32-32 :int64)
(defctype fixed-48-16 fixed-32-32)
(defctype fixed-1-31 :uint32)
(defctype fixed-1-16 :uint32)
(defctype fixed-16-16 :int32)
(defctype fixed fixed-16-16)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun fixed-to-int (fixed)
    (ash fixed -16))
  (defun int-to-fixed (int)
    (ash int 16)))

(defconstant +fixed-e+ 1)
(defconstant +fixed-1+ (int-to-fixed 1))
(defconstant +fixed-1-minus-e+ (- +fixed-1+ +fixed-e+))
(defconstant +fixed-minus-1+ (int-to-fixed -1))
(defconstant +max-fixed-48-16+ #x7FFFFFFF)
(defconstant +min-fixed-48-16+ (- (ash 1 -31)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun fixed-to-double (fixed)
    (/ (float fixed) (float +fixed-1+)))
  (defun double-to-fixed (double)
    (floor (* double #.(float #x10000))))
  (defun fixed-frac (fixed)
    (logand fixed +fixed-1-minus-e+))
  (defun fixed-floor (fixed)
    (logand fixed (lognot +fixed-1-minus-e+)))
  (defun fixed-ceil (fixed)
    (fixed-floor (+ fixed +fixed-1-minus-e+)))
  (defun fixed-fraction (fixed)
    (logand fixed +fixed-1-minus-e+))
  (defun fixed-mod-2 (fixed)
    (declare (ignore fixed))
    (error "Not implemented yet")))

(defcstruct color
  (red :uint16)
  (green :uint16)
  (blue :uint16)
  (alpha :uint16))

(defcstruct point-fixed
  (x fixed)
  (y fixed))

(defcstruct line-fixed
  (p1 (:struct point-fixed))
  (p2 (:struct point-fixed)))

(defcstruct vec
  (vector fixed :count 3))

(defcstruct transform
  (matrix fixed :count 9))

(defcstruct rectangle16
  (x :int16)
  (y :int16)
  (width :uint16)
  (height :uint16))

(defcstruct box16
  (x1 :int16)
  (y1 :int16)
  (x2 :int16)
  (y2 :int16))

(defcstruct region16-data
  (size :long)
  (num-rects :long)
  ;; array of box16 here, not implemented
  )

(defcstruct region16
  (extents (:struct box16))
  (data (:pointer (:struct region16-data))))

(defcstruct gradient-stop
  (x fixed)
  (color (:struct color)))
