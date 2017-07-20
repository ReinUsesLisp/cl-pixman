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

(declaim (inline null-image))
(defun null-image ()
  (null-pointer))

(defun imgcollect (image)
  (collect image #'pixman-image-unref))

(defun image-create-solid-fill (color)
  (imgcollect (pixman-image-create-solid-fill color)))

(defun image-create-linear-gradient (point1 point2 stops)
  (imgcollect (pixman-image-create-linear-gradient
	       point1 point2
	       (build-memory-from-list stops '(:struct gradient-stops))
	       (length stops))))

(defun image-create-conical-gradient (inner outer inner-radius outer-radius
			              stops)
  (imgcollect (pixman-image-create-radial-gradient
               inner outer inner-radius outer-radius
               (build-memory-from-list stops '(:struct gradient-stops))
               (length stops))))

(defun image-create-conical-gradient (center angle stops)
  (imgcollect (pixman-image-create-conical-gradient
               center angle
               (build-memory-from-list stops '(:struct gradient-stops))
               (length stops))))

(defun image-create-bits (format width height bits rowstride-bytes)
  (imgcollect
   (pixman-image-create-bits format width height bits rowstride-bytes)))

(defun image-create-bits-no-clear (format width height bits rowstride-bytes)
  (imgcollect (pixman-image-create-bits-no-clear
	       format width height bits rowstride-bytes)))

(defun image-unref (image)
  (tg:cancel-finalization image)
  (pixman-image-unref image))
