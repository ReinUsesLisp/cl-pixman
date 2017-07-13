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

(defun build-format (bpp type a r g b)
  (logior (ash bpp 24)
          (ash (foreign-enum-value 'type type) 16)
          (ash a 12)
          (ash r 8)
          (ash g 4)
          b))

(defun collect (ptr &optional (function #'foreign-free))
  (let ((address (pointer-address ptr)))
    (tg:finalize ptr
                 (lambda ()
                   (funcall function (make-pointer address))))
    ptr))


