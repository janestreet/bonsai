open! Core
open Bonsai_web
module Record_builder_lib = Record_builder

module type Record_builder = sig
  type 'a profunctor_term

  module Bare : sig
    module Make_creator_types : Record_builder__Record_builder_intf.Make_creator_types
  end

  val field
    :  'field profunctor_term
    -> ('record, 'field) Base.Field.t
    -> ( 'field
       , 'a
       , ('b, 'c) Record_builder_lib.Hlist.cons
       , 'record )
       Bare.Make_creator_types.handle_one_field

  val field'
    :  'field profunctor_term
    -> label_of_field:(('record, 'field) Base.Field.t -> string)
    -> ('record, 'field) Base.Field.t
    -> ( 'field
       , 'a
       , ('b, 'c) Record_builder_lib.Hlist.cons
       , 'record )
       Bare.Make_creator_types.handle_one_field

  val build_for_record
    :  ( 'record
       , ('a, 'b) Record_builder_lib.Hlist.cons
       , 'record )
       Bare.Make_creator_types.handle_all_fields
    -> 'record profunctor_term
end

module type Dynamic_record_builder = sig
  type 'a profunctor_term

  module Bare : sig
    module Make_creator_types : Record_builder__Record_builder_intf.Make_creator_types
  end

  val build_for_record
    :  ('a, _ Record_builder_lib.Hlist.cons, 'a) Bare.Make_creator_types.handle_all_fields
    -> 'a profunctor_term Computation.t

  val field
    :  'a profunctor_term Value.t
    -> ([ `Read | `Set_and_create ], 'b, 'a) Base.Field.t_with_perm
    -> ('a * 'c, 'd * 'e, 'b) Bare.Make_creator_types.accum
    -> ('d * 'e -> 'a) * ('c, 'd * 'e, 'b) Bare.Make_creator_types.accum

  val field'
    :  'a profunctor_term Value.t
    -> label_of_field:(('b, 'a) Base.Field.t -> string)
    -> ([ `Read | `Set_and_create ], 'b, 'a) Base.Field.t_with_perm
    -> ('a * 'c, 'd * 'e, 'b) Bare.Make_creator_types.accum
    -> ('d * 'e -> 'a) * ('c, 'd * 'e, 'b) Bare.Make_creator_types.accum
end
