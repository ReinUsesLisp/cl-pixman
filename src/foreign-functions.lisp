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

;;; misc

(defcfun (blt "pixman_blt") :boolean
  (src-bits (:pointer :uint32)) (dst-bits (:pointer :uint32))
  (src-stride :int) (dst-stride :int)
  (src-bpp :int) (dst-bpp :int)
  (src-x :int) (src-y :int)
  (dst-x :int) (dst-y :int)
  (width :int) (height :int))

(defcfun (pixman-fill "pixman_fill") :boolean
  (bits (:pointer :uint32))
  (stride :int)
  (bpp :int)
  (x :int) (y :int)
  (width :int) (height :int)
  (xor :uint32))

(defcfun "pixman_version" :int)

(defcfun (version-string "pixman_version_string") :string)

;;; transform

(defcfun (transform-init-identity "pixman_transform_init_identity") :void
  (matrix (:pointer (:struct transform))))

(defcfun (transform-point-3d "pixman_transform_point_3d") :boolean
  (transform (:pointer (:struct transform)))
  (vector (:pointer (:struct vec))))

(defcfun (transform-point "pixman_transform_point") :boolean
  (transform (:pointer (:struct transform)))
  (vector (:pointer (:struct vec))))

(defcfun (transform-multiply "pixman_transform_multiply") :boolean
  (dst (:pointer (:struct transform)))
  (l (:pointer (:struct transform)))
  (r (:pointer (:struct transform))))

(defcfun (transform-init-scale "pixman_transform_init_scale") :void
  (transform (:pointer (:struct transform)))
  (x fixed)
  (y fixed))

(defcfun (transform-scale "pixman_transform_scale") :boolean
  (forward (:pointer (:struct transform)))
  (reverse (:pointer (:struct transform)))
  (x fixed)
  (y fixed))

(defcfun (transform-init-rotate "pixman_transform_init_rotate") :void
  (transform (:pointer (:struct transform)))
  (cos fixed)
  (sin fixed))

(defcfun (trasnform-rotate "pixman_transform_rotate") :boolean
  (forward (:pointer (:struct transform)))
  (reverse (:pointer (:struct transform)))
  (cos fixed)
  (sin fixed))

(defcfun (transform-init-translate "pixman_transform_init_translate") :void
  (transform (:pointer (:struct transform)))
  (x fixed)
  (y fixed))

(defcfun (transform-translate "pixman_transform_translate") :boolean
  (forward (:pointer (:struct transform)))
  (reverse (:pointer (:struct transform)))
  (x fixed)
  (y fixed))

(defcfun (transform-bounds "pixman_transform_bounds") :boolean
  (matrix (:pointer (:struct transform)))
  (box (:pointer (:struct box16))))

(defcfun (transform-invert "pixman_transform_invert") :boolean
  (dst (:pointer (:struct transform)))
  (src (:pointer (:struct transform))))

(defcfun (transform-identity-p "pixman_transform_is_identity") :boolean
  (transform (:pointer (:struct transform))))

(defcfun (transform-scale-p "pixman_transform_is_scale") :boolean
  (transform (:pointer (:struct transform))))

(defcfun (transform-int-translate-p "pixman_transform_is_int_translate") :boolean
  (transform (:pointer (:struct transform))))

(defcfun (transform-inverse-p "pixman_transform_is_inverse") :boolean
  (transform-a (:pointer (:struct transform)))
  (transform-b (:pointer (:struct transform))))

;;; region

(defcfun "pixman_region_init" :void
  (region (:pointer (:struct region16))))

(defcfun "pixman_region_init_rect" :void
  (region (:pointer (:struct region16)))
  (x :int)
  (y :int)
  (width :uint)
  (height :uint))

(defcfun "pixman_region_init_rects" :boolean
  (region (:pointer (:struct region16)))
  (boxes (:pointer (:struct box16)))
  (count :int))

(defcfun "pixman_region_init_with_extents" :void
  (region (:pointer (:struct region16)))
  (extents (:pointer (:struct box16))))

(defcfun "pixman_region_init_from_image" :void
  (region (:pointer (:struct region16)))
  (image image))

(defcfun "pixman_region_fini" :void
  (region (:pointer (:struct region16))))

;;; manipulation

(defcfun "pixman_region_translate" :void
  (region (:pointer (:struct region16)))
  (x :int)
  (y :int))

(defcfun "pixman_region_copy" :boolean
  (dest (:pointer (:struct region16)))
  (source (:pointer (:struct region16))))

(defcfun "pixman_region_intersect" :boolean
  (new-region (:pointer (:struct region16)))
  (region1 (:pointer (:struct region16)))
  (region2 (:pointer (:struct region16))))

(defcfun "pixman_region_union" :boolean
  (new-region (:pointer (:struct region16)))
  (region1 (:pointer (:struct region16)))
  (region2 (:pointer (:struct region16))))

(defcfun "pixman_region_union_rect" :boolean
  (dest (:pointer (:struct region16)))
  (source (:pointer (:struct region16)))
  (x :int) (y :int)
  (width :uint) (height :uint))

(defcfun "pixman_region_intersect_rect" :boolean
  (dest (:pointer (:struct region16)))
  (source (:pointer (:struct region16)))
  (x :int) (y :int)
  (width :uint) (height :uint))

(defcfun "pixman_region_subtract" :boolean
  (reg-d (:pointer (:struct region16)))
  (reg-m (:pointer (:struct region16)))
  (reg-s (:pointer (:struct region16))))

(defcfun "pixman_region_inverse" :boolean
  (new-region (:pointer (:struct region16)))
  (region (:pointer (:struct region16)))
  (inv-rect (:pointer (:struct box16))))

(defcfun (region-contains-point-p "pixman_region_contains_point") :boolean
  (region (:pointer (:struct region16)))
  (x :int) (y :int)
  (box (:pointer (:struct box16))))

(defcfun (region-contains-rectangle "pixman_region_contains_rectangle")
    region-overlap
  (region (:pointer (:struct region16)))
  (prect (:pointer (:struct box16))))

(defcfun "pixman_region_not_empty" :boolean
  (region (:pointer (:struct region16))))

(defcfun (region-extents "pixman_region_extents") (:pointer (:struct box16))
  (region (:pointer (:struct region16))))

(defcfun (region-num-rects "pixman_region_n_rects") :int
  (region (:pointer (:struct region16))))

(defcfun "pixman_region_rectangles" (:pointer (:struct box16))
  (region (:pointer (:struct region16)))
  (n-rects (:pointer :int)))

(defcfun "pixman_region_equal" :boolean
  (region1 (:pointer (:struct region16)))
  (region2 (:pointer (:struct region16))))

(defcfun (region-selfcheck "pixman_region_selfcheck") :boolean
  (region (:pointer (:struct region16))))

(defcfun (region-reset "pixman_region_reset") :void
  (region (:pointer (:struct region16)))
  (box (:pointer (:struct box16))))

(defcfun (region-clear "pixman_region_clear") :void
  (region (:pointer (:struct region16))))

;;; image

(defcfun (format-supported-destination "pixman_format_supported_destination")
    :boolean
  (format format-code))

(defcfun (format-supported-source "pixman_format_supported_source") :boolean
  (format format-code))

(defcfun "pixman_image_create_solid_fill" image
  (color (:pointer (:struct color))))

(defcfun "pixman_image_create_linear_gradient" image
  (p1 (:pointer (:struct point-fixed)))
  (p2 (:pointer (:struct point-fixed)))
  (stops (:pointer (:struct gradient-stop)))
  (num-stops :int))

(defcfun "pixman_image_create_radial_gradient" image
  (inner (:pointer (:struct point-fixed)))
  (outer (:pointer (:struct point-fixed)))
  (inner-radius fixed)
  (outer-radius fixed)
  (stops (:pointer (:struct gradient-stop)))
  (num-stops :int))

;;(defcfun "pixman_image_create_conical_gradient"

(defcfun "pixman_image_create_bits" image
  (format format-code)
  (width :int)
  (height :int)
  (bits (:pointer :uint32))
  (rowstride-bytes :int))

(defcfun "pixman_image_create_bits_no_clear" image
  (format format-code)
  (width :int)
  (height :int)
  (bits (:pointer :uint32))
  (rowstride-bytes :int))

(defcfun (image-ref "pixman_image_ref") image
  (image image))

(defcfun "pixman_image_unref" :boolean
  (image image))

;;; properties

(defcfun (image-get-data "pixman_image_get_data") (:pointer :uint32)
  (image image))

(defcfun (image-get-width "pixman_image_get_width") :int
  (image image))

(defcfun (image-get-height "pixman_image_get_height") :int
  (image image))

(defcfun (image-get-stride "pixman_image_get_stride") :int
  (image image))

(defcfun (image-get-depth "pixman_image_get_depth") :int
  (image image))

(defcfun (image-get-format "pixman_image_get_format") format-code
  (image image))

;;; composite

(defcfun (image-composite "pixman_image_composite") :void
  (op op)
  (src image) (mask image) (dest image)
  (src-x :int16) (src-y :int16)
  (mask-x :int16) (mask-y :int16)
  (dest-x :int16) (dest-y :int16)
  (width :uint16) (height :uint16))
