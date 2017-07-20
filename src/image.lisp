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

(defun image-create-solid-fill (color)
  (collect (pixman-image-create-solid-fill color)
    #'pixman-image-unref))

(defun image-create-bits (format width height bits rowstride-bytes)
  (collect (pixman-image-create-bits format width height bits rowstride-bytes)
    #'pixman-image-unref))

(defun image-create-bits-no-clear (format width height bits rowstride-bytes)
  (collect (pixman-image-create-bits-no-clear format width height bits rowstride-bytes)
    #'pixman-image-unref))

(defun image-unref (image)
  (tg:cancel-finalization image)
  (pixman-image-unref image))
