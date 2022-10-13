open! Core
open Bonsai_web

(** This library provides an alternative to an HTML select.
    Notable features :
    - Allows users to type in text to filter results
    - Bounded number of DOM appends per search
    - Autocomplete can display the choice as well as associated metadata
*)

module type Item = sig
  type t [@@deriving compare, sexp, equal]

  val to_string : t -> string
end

module Input : sig
  type 'a t =
    { choices : 'a list
    ; on_select : 'a -> unit Vdom.Effect.t
    }
  [@@deriving fields]
end

val create
  :  (module Item with type t = 'item)
  -> ?max_query_results:int
  (** If the input matches more than [max_query_results], only that many will be shown and
      the user will have a link to view more. Default is 10. *)
  -> ?additional_query_results_on_click:int
  (** If the user clicks on the link to view more query results,
      [additional_query_results_on_click] more are shown. Default is 10. *)
  -> ?width:Css_gen.Length.t
  -> ?placeholder:string
  -> ?initial_query:string
  -> ?wrap_search_bar:(Vdom.Node.t -> Vdom.Node.t)
  -> ?autocomplete_item:('item -> Vdom.Node.t)
  (** Controls how items are displayed in the autocomplete box. *)
  -> ?filter_choice:(input_text:string -> 'item -> bool)
  (** Controls whether an item is considered to match a given search text. The default is
      [String.is_substring (Item.to_string item) ~prefix:input_text]. *)
  -> ?score_choice:(input_text:string -> 'item -> int)
  (** Controls whether items are considered to match the given the search text, and also
      how they are sorted. A score of 0 means to omit them from the results.

      *)
  -> ?of_string:(string -> 'item option)
  (** If at any point the user enters a string s such that [of_string s = Some item], then
      [item] is considered immediately selected.

      *)
  -> ?extra_textbox_attr:Vdom.Attr.t
  -> unit
  -> 'item Input.t Value.t
  -> Vdom.Node.t Computation.t
