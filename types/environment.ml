open! Core_kernel

module Incremental = struct
  type ('witness, 'a) t = ('a, 'witness) Incremental.t [@@deriving sexp_of]
end

include Univ_map.Make1 (Univ_map.Type_id_key) (Incremental)
