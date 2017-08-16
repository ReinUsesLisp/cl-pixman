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

(defmacro with-transform ((name) &body body)
  `(let ((,name (make-transform)))
     ,@body))

(defmacro with-transforms (names &body body)
  (if names
      `(with-transform (,(first names))
         (with-transforms ,(rest names)
           ,@body))
      `(progn ,@body)))

(defmacro a-transform (&body body)
  `(with-transform (it)
     ,@body
     it))

(defun make-identity ()
  (a-transform
    (transform-init-identity it)))

(defun make-translate (x y)
  (a-transform
    (transform-init-translate it x y)))

(defun make-scale (x y)
  (a-transform
    (transform-init-scale it x y)))

(defun make-rotate (cos sin)
  (a-transform
    (transform-init-rotate it cos sin)))
