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

(asdf:defsystem #:pixman
  :description "Low-level pixel manipulation."
  :author "ReinUsesLisp <reinuseslisp@airmail.cc>"
  :license "LLGPL"
  :depends-on (:cffi :trivial-garbage)
  :pathname "src"
  :serial t
  :components ((:file "package")
               (:file "helpers")
               (:file "ffi")
               (:file "pixman")))
