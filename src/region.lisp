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

(defmacro let-region ((&optional (var 'region)) &body body)
  `(let ((,var (alloc-region)))
     ,@body
     ,var))

(defun free-region (region)
  (pixman-region-fini region)
  (foreign-free region))

(defun alloc-region ()
  "Always call this function with a pixman_region_init_* call"
  (collect (foreign-alloc '(:struct region16))
    #'free-region))

(defun make-region ()
  (let-region (region)
    (pixman-region-init region)))

(defun make-region-rect (x y width height)
  (declare (type fixnum x y width height))
  (let-region (region)
    (pixman-region-init-rect region x y width height)))

(defun make-region-rects (&rest boxes)
  (declare (type list boxes))
  (loop with length = (length boxes)
     with mem = (foreign-alloc '(:struct box16) :count length)
     for i from 0 by 1
     for box in boxes do
       (setf (mem-aref mem '(:struct box16) i) (mem-aref box '(:struct box16)))
     finally (return-from make-region-rects
               (let-region (region)
                 (check-true (pixman-region-init-rects region mem length))
                 (foreign-free mem)))))

(defun make-region-with-extents (box)
  (let-region (region)
    (pixman-region-init-with-extents region box)))

(defun make-region-init-from-image (image)
  (let-region (region)
    (pixman-region-init-from-image region image)))

(defun region-fini (region)
  (tg:cancel-finalization region)
  (free-region region))
