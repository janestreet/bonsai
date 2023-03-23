open! Core
open! Import

module type S = sig
  module Theme : T

  module Table : sig
    (** A lower-level module for building tables that puts more burden on the user
        to manage the structure of the table, but also provides more freedom
        regarding the structure of the table. *)

    module Header_cell : T
    module Header_row : T
    module Data_row : T
    module Data_cell : T

    val header_cell : ?attrs:Vdom.Attr.t list -> Vdom.Node.t list -> Header_cell.t
    val header_row : ?attrs:Vdom.Attr.t list -> Header_cell.t list -> Header_row.t
    val data_cell : ?attrs:Vdom.Attr.t list -> Vdom.Node.t list -> Data_cell.t
    val data_row : ?attrs:Vdom.Attr.t list -> Data_cell.t list -> Data_row.t

    val table
      :  Theme.t
      -> ?table_attr:Vdom.Attr.t
      -> header_rows:Header_row.t list
      -> data_rows:Data_row.t list
      -> unit
      -> Vdom.Node.t
  end
end
