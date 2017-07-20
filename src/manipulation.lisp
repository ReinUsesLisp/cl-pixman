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

(defmacro define-manipulator (name args destiny-arg &body body)
  (let ((imperative (symbolicate 'region-n name))
        (functional (symbolicate 'region- name)))
    `(progn
       (defun ,imperative ,args
         ,@body
         ,destiny-arg)
       (defun ,functional ,(remove destiny-arg args)
         (,imperative
          ,@(loop for arg in args
               collect (if (eq arg destiny-arg)
                           '(alloc-region)
                           arg)))))))

(defmacro define-binary-manipulator (name args &body body)
  `(defun ,(symbolicate 'region- name) ,args
     (reduce (lambda (region1 region2)
               (let-region (result)
                 ,@body))
             regions)))

(defun region-copy (source)
  (let-region (dest)
    (pixman-region-copy dest source)))

(defun region-ntranslate (region x y)
  (pixman-region-translate region x y)
  region)

(defun region-translate (region x y)
  (region-ntranslate (region-copy region) x y))

(define-binary-manipulator intersect (&rest regions)
  (check-true (pixman-region-intersect result region1 region2)))

(define-binary-manipulator union (&rest regions)
  (check-true (pixman-region-union result region1 region2)))

(define-manipulator union-rect (destiny source x y width height)
    destiny
  (check-true (pixman-region-union-rect destiny source x y width height)))

(define-manipulator intersect-rect (destiny region x y width height)
    destiny
  (check-true (pixman-region-intersect-rect destiny region x y width height)))

(define-binary-manipulator subtract (&rest regions)
  (check-true (pixman-region-subtract result region1 region2)))

(define-manipulator inverse (destiny region inv-rect) destiny
  (check-true (pixman-region-inverse destiny region inv-rect)))

(defun region-empty-p (region)
  (not (pixman-region-not-empty region)))

(defun region-rectangles (region)
  (loop with rects-ptr = (pixman-region-rectangles region (null-pointer))
     for i from 0 to (1- (region-num-rects region))
     collect (prog1 rects-ptr
               (incf-pointer rects-ptr (foreign-type-size '(:struct box16))))))

(defun region-equal (&rest regions)
  (let ((region1 (first regions)))
    (dolist (region2 (rest regions))
      (when (not (pixman-region-equal region1 region2))
        (return-from region-equal nil)))
    t))

;;; this function is not part of pixman's API, but it's handy for debugging
(defun region-print (region &optional (stream *standard-output*))
  (declare (type stream stream))
  (macrolet ((value (var struct slot)
               `(foreign-slot-value ,var '(:struct ,struct) ',slot)))
    (flet ((show-box-extents (box)
             (format stream "~D ~D ~D ~D~%"
                     (value box box16 x1)
                     (value box box16 y1)
                     (value box box16 x2)
                     (value box box16 y2))))
      (format stream "Extents: ")
      (show-box-extents (region-extents region))
      (mapc #'show-box-extents (region-rectangles region)))))

