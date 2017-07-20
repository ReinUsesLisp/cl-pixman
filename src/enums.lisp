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

(defcenum type
  :other :a :argb :abgr :color :gray :yuy2 :yv12 :bgra :rgba :argb-srgb)

(defcenum format-code
  (:a8r8g8b8 #.(build-format 32 :argb 8 8 8 8))
  (:x8r8g8b8 #.(build-format 32 :argb 0 8 8 8))
  (:a8b8g8r8 #.(build-format 32 :abgr 8 8 8 8))
  (:x8b8g8r8 #.(build-format 32 :abgr 0 8 8 8))
  (:b8g8r8a8 #.(build-format 32 :bgra 8 8 8 8))
  (:b8g8r8x8 #.(build-format 32 :bgra 0 8 8 8))
  (:r8g8b8a8 #.(build-format 32 :rgba 8 8 8 8))
  (:r8g8b8x8 #.(build-format 32 :rgba 0 8 8 8))
  (:x14r6g6b6 #.(build-format 32 :argb 0 6 6 6))
  (:x2r10g10b10 #.(build-format 32 :argb 0 10 10 10))
  (:a2r10g10b10 #.(build-format 32 :argb 2 10 10 10))
  (:x2b10g10r10 #.(build-format 32 :abgr 0 10 10 10))
  (:a2b10g10r10 #.(build-format 32 :abgr 2 10 10 10))

  (:a8r8g8b8-srgb #.(build-format 32 :argb-srgb 8 8 8 8))

  (:r8g8b8 #.(build-format 24 :argb 0 8 8 8))
  (:b8g8r8 #.(build-format 24 :abgr 0 8 8 8))
  
  (:r5b6b5 #.(build-format 16 :argb 0 5 6 5))
  (:b5g6r5 #.(build-format 16 :abgr 0 5 6 5))

  (:a1r5g5b5 #.(build-format 16 :argb 1 5 5 5))
  (:x1r5b5g5 #.(build-format 16 :argb 0 5 5 5))
  (:a1b5g5r5 #.(build-format 16 :abgr 1 5 5 5))
  (:x1b5g5r5 #.(build-format 16 :abgr 0 5 5 5))
  (:a4r4g4b4 #.(build-format 16 :argb 4 4 4 4))
  (:x4r4g4b4 #.(build-format 16 :argb 0 4 4 4))
  (:a4b4g4r4 #.(build-format 16 :abgr 4 4 4 4))
  (:x4b4g4r4 #.(build-format 16 :abgr 0 4 4 4))

  (:a8 #.(build-format 8 :a 8 0 0 0))
  (:r3g3b2 #.(build-format 8 :argb 0 3 3 2))
  (:b2g3r3 #.(build-format 8 :abgr 0 3 3 2))
  (:a2r2g2b2 #.(build-format 8 :abgr 2 2 2 2))
  (:a2b2g2r2 #.(build-format 8 :abgr 2 2 2 2))

  (:c8 #.(build-format 8 :color 0 0 0 0))
  (:g8 #.(build-format 8 :gray 0 0 0 0))
  
  (:x4a4 #.(build-format 8 :a 4 0 0 0))
  
  (:x4c4 #.(build-format 8 :color 0 0 0 0))
  (:x4g4 #.(build-format 8 :gray 0 0 0 0))

  (:a4 #.(build-format 4 :a 0 0 0 0))
  (:r1g2b1 #.(build-format 4 :argb 0 1 2 1))
  (:b1g2r1 #.(build-format 4 :abgr 0 1 2 1))
  (:a1r1g1b1 #.(build-format 4 :argb 1 1 1 1))
  (:a1b1g1r1 #.(build-format 4 :abgr 1 1 1 1))

  (:c4 #.(build-format 4 :color 0 0 0 0))
  (:g4 #.(build-format 4 :gray 0 0 0 0))

  (:a1 #.(build-format 1 :a 1 0 0 0))
  
  (:g1 #.(build-format 1 :gray 0 0 0 0))

  (:yuy2 #.(build-format 16 :yuy2 0 0 0 0))
  (:yv12 #.(build-format 12 :yv12 0 0 0 0)))

(defcenum op
  (:clear #x00)
  (:src #x01)
  (:dst #x02)
  (:over #x03)
  (:over-reverse #x04)
  (:in #x05)
  (:in-reverse #x06)
  (:out #x07)
  (:out-reverse #x08)
  (:atop #x09)
  (:atop-reverse #x0a)
  (:xor #x0b)
  (:add #x0c)
  (:saturate #x0d)

  (:disjoint-clear #x10)
  (:disjoint-src #x11)
  (:disjoint-dst #x12)
  (:disjoint-over #x13)
  (:disjoint-over-reverse #x14)
  (:disjoint-in #x15)
  (:disjoint-in-reverse #x16)
  (:disjoint-out #x17)
  (:disjoint-out-reverse #x18)
  (:disjoint-atop #x19)
  (:disjoint-atop-reverse #x1a)
  (:disjoint-xor #x1b)

  (:conjoint-clear #x20)
  (:conjoint-src #x21)
  (:conjoint-dst #x22)
  (:conjoint-over #x23)
  (:conjoint-over-reverse #x24)
  (:conjoint-in #x25)
  (:conjoint-in-reverse #x26)
  (:conjoint-out #x27)
  (:conjoint-out-reverse #x28)
  (:conjoint-atop #x29)
  (:conjoint-atop-reverse #x2a)
  (:conjoint-xor #x2b)

  (:multiply #x30)
  (:screen #x31)
  (:overlay #x32)
  (:darken #x33)
  (:lighten #x34)
  (:color-dodge #x35)
  (:color-burn #x36)
  (:hard-light #x37)
  (:soft-light #x38)
  (:difference #x39)
  (:exclusion #x3a)
  (:hsl-hue #x3b)
  (:hsl-saturation #x3c)
  (:hsl-color #x3d)
  (:hsl-luminosity #x3e))

(defcenum repeat
  :none :normal :pad :reflect)

(defcenum filter
  :fast :good :best :nearest :bilinear :convolution :separable-convolution)

(defcenum region-overlap
  :out :in :part)
