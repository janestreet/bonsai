open Core

module Username = struct
  type t = { username : string } [@@deriving compare, equal, fields, sexp]

  let of_user_info user_info = Fields.create ~username:(User_info.name user_info)
  let to_string t = username t
  let of_string username = Fields.create ~username |> Option.some
end

module Input = struct
  include Bonsai_web_ui_search_bar.Input

  type nonrec t = Username.t t

  let create = Fields.create
end

let component =
  Bonsai_web_ui_search_bar.create (module Username) ~of_string:Username.of_string ()
;;
