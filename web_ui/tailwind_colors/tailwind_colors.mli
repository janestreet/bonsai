(** The color palettes included in the Tailwind CSS library, taken from
    [https://tailwindcss.com/docs/customizing-colors] on 2022-11-21. The larger
    the number on a color, the darker it is.

    This library takes the approach of *not* ascribing semantic value to
    certain colors; instead, it selects several "palettes" of colors which work
    well together, and offloads the semantics of each color to the library or
    application that depends on this one. Thus, this library does not aim to
    provide any consistent design pattern or color conventions. However, a set
    of conventions could easily be created using this set of colors. *)

type t := [ `Hex of string ]

module Hue : sig
  type t =
    [ `slate
    | `gray
    | `zinc
    | `neutral
    | `stone
    | `red
    | `orange
    | `amber
    | `yellow
    | `lime
    | `green
    | `emerald
    | `teal
    | `cyan
    | `sky
    | `blue
    | `indigo
    | `violet
    | `purple
    | `fuchsia
    | `pink
    | `rose
    ]
  [@@deriving enumerate]

  val to_string : t -> string
end

module Brightness : sig
  type t =
    [ `_50
    | `_100
    | `_200
    | `_300
    | `_400
    | `_500
    | `_600
    | `_700
    | `_800
    | `_900
    ]
  [@@deriving enumerate]
end

val create : Hue.t -> Brightness.t -> t

(** The slate palette *)

val slate50 : t
val slate100 : t
val slate200 : t
val slate300 : t
val slate400 : t
val slate500 : t
val slate600 : t
val slate700 : t
val slate800 : t
val slate900 : t

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

(** The zinc palette *)

val zinc50 : t
val zinc100 : t
val zinc200 : t
val zinc300 : t
val zinc400 : t
val zinc500 : t
val zinc600 : t
val zinc700 : t
val zinc800 : t
val zinc900 : t

(** The neutral palette *)

val neutral50 : t
val neutral100 : t
val neutral200 : t
val neutral300 : t
val neutral400 : t
val neutral500 : t
val neutral600 : t
val neutral700 : t
val neutral800 : t
val neutral900 : t

(** The stone palette *)

val stone50 : t
val stone100 : t
val stone200 : t
val stone300 : t
val stone400 : t
val stone500 : t
val stone600 : t
val stone700 : t
val stone800 : t
val stone900 : t

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

(** The orange palette *)

val orange50 : t
val orange100 : t
val orange200 : t
val orange300 : t
val orange400 : t
val orange500 : t
val orange600 : t
val orange700 : t
val orange800 : t
val orange900 : t

(** The amber palette *)

val amber50 : t
val amber100 : t
val amber200 : t
val amber300 : t
val amber400 : t
val amber500 : t
val amber600 : t
val amber700 : t
val amber800 : t
val amber900 : t

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

(** The lime palette *)

val lime50 : t
val lime100 : t
val lime200 : t
val lime300 : t
val lime400 : t
val lime500 : t
val lime600 : t
val lime700 : t
val lime800 : t
val lime900 : t

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

(** The emerald palette *)

val emerald50 : t
val emerald100 : t
val emerald200 : t
val emerald300 : t
val emerald400 : t
val emerald500 : t
val emerald600 : t
val emerald700 : t
val emerald800 : t
val emerald900 : t

(** The teal palette *)

val teal50 : t
val teal100 : t
val teal200 : t
val teal300 : t
val teal400 : t
val teal500 : t
val teal600 : t
val teal700 : t
val teal800 : t
val teal900 : t

(** The cyan palette *)

val cyan50 : t
val cyan100 : t
val cyan200 : t
val cyan300 : t
val cyan400 : t
val cyan500 : t
val cyan600 : t
val cyan700 : t
val cyan800 : t
val cyan900 : t

(** The sky palette *)

val sky50 : t
val sky100 : t
val sky200 : t
val sky300 : t
val sky400 : t
val sky500 : t
val sky600 : t
val sky700 : t
val sky800 : t
val sky900 : t

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

(** The violet palette *)

val violet50 : t
val violet100 : t
val violet200 : t
val violet300 : t
val violet400 : t
val violet500 : t
val violet600 : t
val violet700 : t
val violet800 : t
val violet900 : t

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

(** The fuchsia palette *)

val fuchsia50 : t
val fuchsia100 : t
val fuchsia200 : t
val fuchsia300 : t
val fuchsia400 : t
val fuchsia500 : t
val fuchsia600 : t
val fuchsia700 : t
val fuchsia800 : t
val fuchsia900 : t

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

(** The rose palette *)

val rose50 : t
val rose100 : t
val rose200 : t
val rose300 : t
val rose400 : t
val rose500 : t
val rose600 : t
val rose700 : t
val rose800 : t
val rose900 : t

module Stable : sig
  module Hue : sig
    module V1 : sig
      type t = Hue.t [@@deriving bin_io, compare, enumerate, equal, sexp, sexp_grammar]
    end
  end
end
