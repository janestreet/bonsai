module Stable = struct
  module Hue = struct
    module V1 = struct
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
      [@@deriving bin_io, compare, enumerate, equal, sexp, sexp_grammar]
    end
  end
end

module Hue = struct
  module T = struct
    type t = Stable.Hue.V1.t [@@deriving enumerate, sexp_of]
  end

  include T
  include Enum.Make_stringable (T)
end

module Brightness = struct
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

(** slate palette *)

let slate50 = `Hex "#F8FAFC"
let slate100 = `Hex "#F1F5F9"
let slate200 = `Hex "#E2E8F0"
let slate300 = `Hex "#CBD5E1"
let slate400 = `Hex "#94A3B8"
let slate500 = `Hex "#64748B"
let slate600 = `Hex "#475569"
let slate700 = `Hex "#334155"
let slate800 = `Hex "#1E293B"
let slate900 = `Hex "#0F172A"

(** gray palette *)

let gray50 = `Hex "#F9FAFB"
let gray100 = `Hex "#F3F4F6"
let gray200 = `Hex "#E5E7EB"
let gray300 = `Hex "#D1D5DB"
let gray400 = `Hex "#9CA3AF"
let gray500 = `Hex "#6B7280"
let gray600 = `Hex "#4B5563"
let gray700 = `Hex "#374151"
let gray800 = `Hex "#1F2937"
let gray900 = `Hex "#111827"

(** zinc palette *)

let zinc50 = `Hex "#FAFAFA"
let zinc100 = `Hex "#F4F4F5"
let zinc200 = `Hex "#E4E4E7"
let zinc300 = `Hex "#D4D4D8"
let zinc400 = `Hex "#A1A1AA"
let zinc500 = `Hex "#71717A"
let zinc600 = `Hex "#52525B"
let zinc700 = `Hex "#3F3F46"
let zinc800 = `Hex "#27272A"
let zinc900 = `Hex "#18181B"

(** neutral palette *)

let neutral50 = `Hex "#FAFAFA"
let neutral100 = `Hex "#F5F5F5"
let neutral200 = `Hex "#E5E5E5"
let neutral300 = `Hex "#D4D4D4"
let neutral400 = `Hex "#A3A3A3"
let neutral500 = `Hex "#737373"
let neutral600 = `Hex "#525252"
let neutral700 = `Hex "#404040"
let neutral800 = `Hex "#262626"
let neutral900 = `Hex "#171717"

(** stone palette *)

let stone50 = `Hex "#FAFAF9"
let stone100 = `Hex "#F5F5F4"
let stone200 = `Hex "#E7E5E4"
let stone300 = `Hex "#D6D3D1"
let stone400 = `Hex "#A8A29E"
let stone500 = `Hex "#78716C"
let stone600 = `Hex "#57534E"
let stone700 = `Hex "#44403C"
let stone800 = `Hex "#292524"
let stone900 = `Hex "#1C1917"

(** red palette *)

let red50 = `Hex "#FEF2F2"
let red100 = `Hex "#FEE2E2"
let red200 = `Hex "#FECACA"
let red300 = `Hex "#FCA5A5"
let red400 = `Hex "#F87171"
let red500 = `Hex "#EF4444"
let red600 = `Hex "#DC2626"
let red700 = `Hex "#B91C1C"
let red800 = `Hex "#991B1B"
let red900 = `Hex "#7F1D1D"

(** orange palette *)

let orange50 = `Hex "#FFF7ED"
let orange100 = `Hex "#FFEDD5"
let orange200 = `Hex "#FED7AA"
let orange300 = `Hex "#FDBA74"
let orange400 = `Hex "#FB923C"
let orange500 = `Hex "#F97316"
let orange600 = `Hex "#EA580C"
let orange700 = `Hex "#C2410C"
let orange800 = `Hex "#9A3412"
let orange900 = `Hex "#7C2D12"

(** amber palette *)

let amber50 = `Hex "#FFFBEB"
let amber100 = `Hex "#FEF3C7"
let amber200 = `Hex "#FDE68A"
let amber300 = `Hex "#FCD34D"
let amber400 = `Hex "#FBBF24"
let amber500 = `Hex "#F59E0B"
let amber600 = `Hex "#D97706"
let amber700 = `Hex "#B45309"
let amber800 = `Hex "#92400E"
let amber900 = `Hex "#78350F"

(** yellow palette *)

let yellow50 = `Hex "#FEFCE8"
let yellow100 = `Hex "#FEF9C3"
let yellow200 = `Hex "#FEF08A"
let yellow300 = `Hex "#FDE047"
let yellow400 = `Hex "#FACC15"
let yellow500 = `Hex "#EAB308"
let yellow600 = `Hex "#CA8A04"
let yellow700 = `Hex "#A16207"
let yellow800 = `Hex "#854D0E"
let yellow900 = `Hex "#713F12"

(** lime palette *)

let lime50 = `Hex "#F7FEE7"
let lime100 = `Hex "#ECFCCB"
let lime200 = `Hex "#D9F99D"
let lime300 = `Hex "#BEF264"
let lime400 = `Hex "#A3E635"
let lime500 = `Hex "#84CC16"
let lime600 = `Hex "#65A30D"
let lime700 = `Hex "#4D7C0F"
let lime800 = `Hex "#3F6212"
let lime900 = `Hex "#365314"

(** green palette *)

let green50 = `Hex "#F0FDF4"
let green100 = `Hex "#DCFCE7"
let green200 = `Hex "#BBF7D0"
let green300 = `Hex "#86EFAC"
let green400 = `Hex "#4ADE80"
let green500 = `Hex "#22C55E"
let green600 = `Hex "#16A34A"
let green700 = `Hex "#15803D"
let green800 = `Hex "#166534"
let green900 = `Hex "#14532D"

(** emerald palette *)

let emerald50 = `Hex "#ECFDF5"
let emerald100 = `Hex "#D1FAE5"
let emerald200 = `Hex "#A7F3D0"
let emerald300 = `Hex "#6EE7B7"
let emerald400 = `Hex "#34D399"
let emerald500 = `Hex "#10B981"
let emerald600 = `Hex "#059669"
let emerald700 = `Hex "#047857"
let emerald800 = `Hex "#065F46"
let emerald900 = `Hex "#064E3B"

(** teal palette *)

let teal50 = `Hex "#F0FDFA"
let teal100 = `Hex "#CCFBF1"
let teal200 = `Hex "#99F6E4"
let teal300 = `Hex "#5EEAD4"
let teal400 = `Hex "#2DD4BF"
let teal500 = `Hex "#14B8A6"
let teal600 = `Hex "#0D9488"
let teal700 = `Hex "#0F766E"
let teal800 = `Hex "#115E59"
let teal900 = `Hex "#134E4A"

(** cyan palette *)

let cyan50 = `Hex "#ECFEFF"
let cyan100 = `Hex "#CFFAFE"
let cyan200 = `Hex "#A5F3FC"
let cyan300 = `Hex "#67E8F9"
let cyan400 = `Hex "#22D3EE"
let cyan500 = `Hex "#06B6D4"
let cyan600 = `Hex "#0891B2"
let cyan700 = `Hex "#0E7490"
let cyan800 = `Hex "#155E75"
let cyan900 = `Hex "#164E63"

(** sky palette *)

let sky50 = `Hex "#F0F9FF"
let sky100 = `Hex "#E0F2FE"
let sky200 = `Hex "#BAE6FD"
let sky300 = `Hex "#7DD3FC"
let sky400 = `Hex "#38BDF8"
let sky500 = `Hex "#0EA5E9"
let sky600 = `Hex "#0284C7"
let sky700 = `Hex "#0369A1"
let sky800 = `Hex "#075985"
let sky900 = `Hex "#0C4A6E"

(** blue palette *)

let blue50 = `Hex "#EFF6FF"
let blue100 = `Hex "#DBEAFE"
let blue200 = `Hex "#BFDBFE"
let blue300 = `Hex "#93C5FD"
let blue400 = `Hex "#60A5FA"
let blue500 = `Hex "#3B82F6"
let blue600 = `Hex "#2563EB"
let blue700 = `Hex "#1D4ED8"
let blue800 = `Hex "#1E40AF"
let blue900 = `Hex "#1E3A8A"

(** indigo palette *)

let indigo50 = `Hex "#EEF2FF"
let indigo100 = `Hex "#E0E7FF"
let indigo200 = `Hex "#C7D2FE"
let indigo300 = `Hex "#A5B4FC"
let indigo400 = `Hex "#818CF8"
let indigo500 = `Hex "#6366F1"
let indigo600 = `Hex "#4F46E5"
let indigo700 = `Hex "#4338CA"
let indigo800 = `Hex "#3730A3"
let indigo900 = `Hex "#312E81"

(** violet palette *)

let violet50 = `Hex "#F5F3FF"
let violet100 = `Hex "#EDE9FE"
let violet200 = `Hex "#DDD6FE"
let violet300 = `Hex "#C4B5FD"
let violet400 = `Hex "#A78BFA"
let violet500 = `Hex "#8B5CF6"
let violet600 = `Hex "#7C3AED"
let violet700 = `Hex "#6D28D9"
let violet800 = `Hex "#5B21B6"
let violet900 = `Hex "#4C1D95"

(** purple palette *)

let purple50 = `Hex "#FAF5FF"
let purple100 = `Hex "#F3E8FF"
let purple200 = `Hex "#E9D5FF"
let purple300 = `Hex "#D8B4FE"
let purple400 = `Hex "#C084FC"
let purple500 = `Hex "#A855F7"
let purple600 = `Hex "#9333EA"
let purple700 = `Hex "#7E22CE"
let purple800 = `Hex "#6B21A8"
let purple900 = `Hex "#581C87"

(** fuchsia palette *)

let fuchsia50 = `Hex "#FDF4FF"
let fuchsia100 = `Hex "#FAE8FF"
let fuchsia200 = `Hex "#F5D0FE"
let fuchsia300 = `Hex "#F0ABFC"
let fuchsia400 = `Hex "#E879F9"
let fuchsia500 = `Hex "#D946EF"
let fuchsia600 = `Hex "#C026D3"
let fuchsia700 = `Hex "#A21CAF"
let fuchsia800 = `Hex "#86198F"
let fuchsia900 = `Hex "#701A75"

(** pink palette *)

let pink50 = `Hex "#FDF2F8"
let pink100 = `Hex "#FCE7F3"
let pink200 = `Hex "#FBCFE8"
let pink300 = `Hex "#F9A8D4"
let pink400 = `Hex "#F472B6"
let pink500 = `Hex "#EC4899"
let pink600 = `Hex "#DB2777"
let pink700 = `Hex "#BE185D"
let pink800 = `Hex "#9D174D"
let pink900 = `Hex "#831843"

(** rose palette *)

let rose50 = `Hex "#FFF1F2"
let rose100 = `Hex "#FFE4E6"
let rose200 = `Hex "#FECDD3"
let rose300 = `Hex "#FDA4AF"
let rose400 = `Hex "#FB7185"
let rose500 = `Hex "#F43F5E"
let rose600 = `Hex "#E11D48"
let rose700 = `Hex "#BE123C"
let rose800 = `Hex "#9F1239"
let rose900 = `Hex "#881337"

let create (hue : Hue.t) (brightness : Brightness.t) =
  match hue with
  | `slate ->
    (match brightness with
     | `_50 -> slate50
     | `_100 -> slate100
     | `_200 -> slate200
     | `_300 -> slate300
     | `_400 -> slate400
     | `_500 -> slate500
     | `_600 -> slate600
     | `_700 -> slate700
     | `_800 -> slate800
     | `_900 -> slate900)
  | `gray ->
    (match brightness with
     | `_50 -> gray50
     | `_100 -> gray100
     | `_200 -> gray200
     | `_300 -> gray300
     | `_400 -> gray400
     | `_500 -> gray500
     | `_600 -> gray600
     | `_700 -> gray700
     | `_800 -> gray800
     | `_900 -> gray900)
  | `zinc ->
    (match brightness with
     | `_50 -> zinc50
     | `_100 -> zinc100
     | `_200 -> zinc200
     | `_300 -> zinc300
     | `_400 -> zinc400
     | `_500 -> zinc500
     | `_600 -> zinc600
     | `_700 -> zinc700
     | `_800 -> zinc800
     | `_900 -> zinc900)
  | `neutral ->
    (match brightness with
     | `_50 -> neutral50
     | `_100 -> neutral100
     | `_200 -> neutral200
     | `_300 -> neutral300
     | `_400 -> neutral400
     | `_500 -> neutral500
     | `_600 -> neutral600
     | `_700 -> neutral700
     | `_800 -> neutral800
     | `_900 -> neutral900)
  | `stone ->
    (match brightness with
     | `_50 -> stone50
     | `_100 -> stone100
     | `_200 -> stone200
     | `_300 -> stone300
     | `_400 -> stone400
     | `_500 -> stone500
     | `_600 -> stone600
     | `_700 -> stone700
     | `_800 -> stone800
     | `_900 -> stone900)
  | `red ->
    (match brightness with
     | `_50 -> red50
     | `_100 -> red100
     | `_200 -> red200
     | `_300 -> red300
     | `_400 -> red400
     | `_500 -> red500
     | `_600 -> red600
     | `_700 -> red700
     | `_800 -> red800
     | `_900 -> red900)
  | `orange ->
    (match brightness with
     | `_50 -> orange50
     | `_100 -> orange100
     | `_200 -> orange200
     | `_300 -> orange300
     | `_400 -> orange400
     | `_500 -> orange500
     | `_600 -> orange600
     | `_700 -> orange700
     | `_800 -> orange800
     | `_900 -> orange900)
  | `amber ->
    (match brightness with
     | `_50 -> amber50
     | `_100 -> amber100
     | `_200 -> amber200
     | `_300 -> amber300
     | `_400 -> amber400
     | `_500 -> amber500
     | `_600 -> amber600
     | `_700 -> amber700
     | `_800 -> amber800
     | `_900 -> amber900)
  | `yellow ->
    (match brightness with
     | `_50 -> yellow50
     | `_100 -> yellow100
     | `_200 -> yellow200
     | `_300 -> yellow300
     | `_400 -> yellow400
     | `_500 -> yellow500
     | `_600 -> yellow600
     | `_700 -> yellow700
     | `_800 -> yellow800
     | `_900 -> yellow900)
  | `lime ->
    (match brightness with
     | `_50 -> lime50
     | `_100 -> lime100
     | `_200 -> lime200
     | `_300 -> lime300
     | `_400 -> lime400
     | `_500 -> lime500
     | `_600 -> lime600
     | `_700 -> lime700
     | `_800 -> lime800
     | `_900 -> lime900)
  | `green ->
    (match brightness with
     | `_50 -> green50
     | `_100 -> green100
     | `_200 -> green200
     | `_300 -> green300
     | `_400 -> green400
     | `_500 -> green500
     | `_600 -> green600
     | `_700 -> green700
     | `_800 -> green800
     | `_900 -> green900)
  | `emerald ->
    (match brightness with
     | `_50 -> emerald50
     | `_100 -> emerald100
     | `_200 -> emerald200
     | `_300 -> emerald300
     | `_400 -> emerald400
     | `_500 -> emerald500
     | `_600 -> emerald600
     | `_700 -> emerald700
     | `_800 -> emerald800
     | `_900 -> emerald900)
  | `teal ->
    (match brightness with
     | `_50 -> teal50
     | `_100 -> teal100
     | `_200 -> teal200
     | `_300 -> teal300
     | `_400 -> teal400
     | `_500 -> teal500
     | `_600 -> teal600
     | `_700 -> teal700
     | `_800 -> teal800
     | `_900 -> teal900)
  | `cyan ->
    (match brightness with
     | `_50 -> cyan50
     | `_100 -> cyan100
     | `_200 -> cyan200
     | `_300 -> cyan300
     | `_400 -> cyan400
     | `_500 -> cyan500
     | `_600 -> cyan600
     | `_700 -> cyan700
     | `_800 -> cyan800
     | `_900 -> cyan900)
  | `sky ->
    (match brightness with
     | `_50 -> sky50
     | `_100 -> sky100
     | `_200 -> sky200
     | `_300 -> sky300
     | `_400 -> sky400
     | `_500 -> sky500
     | `_600 -> sky600
     | `_700 -> sky700
     | `_800 -> sky800
     | `_900 -> sky900)
  | `blue ->
    (match brightness with
     | `_50 -> blue50
     | `_100 -> blue100
     | `_200 -> blue200
     | `_300 -> blue300
     | `_400 -> blue400
     | `_500 -> blue500
     | `_600 -> blue600
     | `_700 -> blue700
     | `_800 -> blue800
     | `_900 -> blue900)
  | `indigo ->
    (match brightness with
     | `_50 -> indigo50
     | `_100 -> indigo100
     | `_200 -> indigo200
     | `_300 -> indigo300
     | `_400 -> indigo400
     | `_500 -> indigo500
     | `_600 -> indigo600
     | `_700 -> indigo700
     | `_800 -> indigo800
     | `_900 -> indigo900)
  | `violet ->
    (match brightness with
     | `_50 -> violet50
     | `_100 -> violet100
     | `_200 -> violet200
     | `_300 -> violet300
     | `_400 -> violet400
     | `_500 -> violet500
     | `_600 -> violet600
     | `_700 -> violet700
     | `_800 -> violet800
     | `_900 -> violet900)
  | `purple ->
    (match brightness with
     | `_50 -> purple50
     | `_100 -> purple100
     | `_200 -> purple200
     | `_300 -> purple300
     | `_400 -> purple400
     | `_500 -> purple500
     | `_600 -> purple600
     | `_700 -> purple700
     | `_800 -> purple800
     | `_900 -> purple900)
  | `fuchsia ->
    (match brightness with
     | `_50 -> fuchsia50
     | `_100 -> fuchsia100
     | `_200 -> fuchsia200
     | `_300 -> fuchsia300
     | `_400 -> fuchsia400
     | `_500 -> fuchsia500
     | `_600 -> fuchsia600
     | `_700 -> fuchsia700
     | `_800 -> fuchsia800
     | `_900 -> fuchsia900)
  | `pink ->
    (match brightness with
     | `_50 -> pink50
     | `_100 -> pink100
     | `_200 -> pink200
     | `_300 -> pink300
     | `_400 -> pink400
     | `_500 -> pink500
     | `_600 -> pink600
     | `_700 -> pink700
     | `_800 -> pink800
     | `_900 -> pink900)
  | `rose ->
    (match brightness with
     | `_50 -> rose50
     | `_100 -> rose100
     | `_200 -> rose200
     | `_300 -> rose300
     | `_400 -> rose400
     | `_500 -> rose500
     | `_600 -> rose600
     | `_700 -> rose700
     | `_800 -> rose800
     | `_900 -> rose900)
;;
