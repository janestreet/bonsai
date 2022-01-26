(** The color palettes included in the Tailwind CSS library, taken from
    [https://tailwindcss.com/docs/customizing-colors] on 2021-02-13. The larger
    the number on a color, the darker it is.

    This library takes the approach of *not* ascribing semantic value to
    certain colors; instead, it selects several "palettes" of colors which work
    well together, and offloads the semantics of each color to the library or
    application that depends on this one. Thus, this library does not aim to
    provide any consistent design pattern or color conventions. However, a set
    of conventions could easily be created using this set of colors. *)

type t := [ `Hex of string ]

(** The gray palette *)

val gray50 : t
val gray100 : t
val gray200 : t
val gray300 : t
val gray400 : t
val gray500 : t
val gray600 : t
val gray700 : t
val gray800 : t
val gray900 : t

(** The red palette *)

val red50 : t
val red100 : t
val red200 : t
val red300 : t
val red400 : t
val red500 : t
val red600 : t
val red700 : t
val red800 : t
val red900 : t

(** The yellow palette *)

val yellow50 : t
val yellow100 : t
val yellow200 : t
val yellow300 : t
val yellow400 : t
val yellow500 : t
val yellow600 : t
val yellow700 : t
val yellow800 : t
val yellow900 : t

(** The green palette *)

val green50 : t
val green100 : t
val green200 : t
val green300 : t
val green400 : t
val green500 : t
val green600 : t
val green700 : t
val green800 : t
val green900 : t

(** The blue palette *)

val blue50 : t
val blue100 : t
val blue200 : t
val blue300 : t
val blue400 : t
val blue500 : t
val blue600 : t
val blue700 : t
val blue800 : t
val blue900 : t

(** The indigo palette *)

val indigo50 : t
val indigo100 : t
val indigo200 : t
val indigo300 : t
val indigo400 : t
val indigo500 : t
val indigo600 : t
val indigo700 : t
val indigo800 : t
val indigo900 : t

(** The purple palette *)

val purple50 : t
val purple100 : t
val purple200 : t
val purple300 : t
val purple400 : t
val purple500 : t
val purple600 : t
val purple700 : t
val purple800 : t
val purple900 : t

(** The pink palette *)

val pink50 : t
val pink100 : t
val pink200 : t
val pink300 : t
val pink400 : t
val pink500 : t
val pink600 : t
val pink700 : t
val pink800 : t
val pink900 : t
