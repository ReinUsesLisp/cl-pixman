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

(defmacro define-maker ((name &optional (type name)) vars &body prebody)
  (with-gensyms (res)
    `(defun ,(symbolicate 'make- name) ,vars
       ,@prebody
       (let ((,res (foreign-alloc '(:struct ,type))))
         ,@(loop for slot in vars
              collect `(setf (foreign-slot-value ,res '(:struct ,type) ',slot)
                             ,slot))
         (collect ,res)))))

(define-maker (box box16) (x1 y1 x2 y2)
  (declare (type (signed-byte 16) x1 y1 x2 y2)))

(define-maker (color) (red green blue alpha)
  (declare (type (unsigned-byte 16) red green blue alpha)))

(define-maker (transform) ())

(define-maker (gradient-stop) (x color))
