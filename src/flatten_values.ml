open! Core
open! Import

let value_map (type a) ({ value; here; id } : a Value.t) =
  let value =
    match value with
    | Map
        { f
        ; t =
            { value =
                Both
                  ( t1
                  , { value =
                        Both
                          ( t2
                          , { value =
                                Both
                                  ( t3
                                  , { value =
                                        Both
                                          ( t4
                                          , { value =
                                                Both (t5, { value = Both (t6, t7); _ })
                                            ; _
                                            } )
                                    ; _
                                    } )
                            ; _
                            } )
                    ; _
                    } )
            ; _
            }
        } ->
      Value.Map7
        { f = (fun t1 t2 t3 t4 t5 t6 t7 -> f (t1, (t2, (t3, (t4, (t5, (t6, t7)))))))
        ; t1
        ; t2
        ; t3
        ; t4
        ; t5
        ; t6
        ; t7
        }
    | Map
        { f
        ; t =
            { value =
                Both
                  ( t1
                  , { value =
                        Both
                          ( t2
                          , { value =
                                Both
                                  ( t3
                                  , { value = Both (t4, { value = Both (t5, t6); _ }); _ }
                                  )
                            ; _
                            } )
                    ; _
                    } )
            ; _
            }
        } ->
      Map6
        { f = (fun t1 t2 t3 t4 t5 t6 -> f (t1, (t2, (t3, (t4, (t5, t6))))))
        ; t1
        ; t2
        ; t3
        ; t4
        ; t5
        ; t6
        }
    | Map
        { f
        ; t =
            { value =
                Both
                  ( t1
                  , { value =
                        Both (t2, { value = Both (t3, { value = Both (t4, t5); _ }); _ })
                    ; _
                    } )
            ; _
            }
        } ->
      Map5
        { f = (fun t1 t2 t3 t4 t5 -> f (t1, (t2, (t3, (t4, t5))))); t1; t2; t3; t4; t5 }
    | Map
        { f
        ; t =
            { value = Both (t1, { value = Both (t2, { value = Both (t3, t4); _ }); _ })
            ; _
            }
        } -> Map4 { f = (fun t1 t2 t3 t4 -> f (t1, (t2, (t3, t4)))); t1; t2; t3; t4 }
    | Map { f; t = { value = Both (t1, { value = Both (t2, t3); _ }); _ } } ->
      Map3 { f = (fun t1 t2 t3 -> f (t1, (t2, t3))); t1; t2; t3 }
    | Map { f; t = { value = Both (t1, t2); _ } } ->
      Map2 { f = (fun t1 t2 -> f (t1, t2)); t1; t2 }
    | Both
        ( t1
        , { value =
              Both
                ( t2
                , { value =
                      Both
                        ( t3
                        , { value =
                              Both
                                ( t4
                                , { value = Both (t5, { value = Both (t6, t7); _ }); _ }
                                )
                          ; _
                          } )
                  ; _
                  } )
          ; _
          } ) ->
      Map7
        { f = (fun t1 t2 t3 t4 t5 t6 t7 -> t1, (t2, (t3, (t4, (t5, (t6, t7))))))
        ; t1
        ; t2
        ; t3
        ; t4
        ; t5
        ; t6
        ; t7
        }
    | Both
        ( t1
        , { value =
              Both
                ( t2
                , { value =
                      Both (t3, { value = Both (t4, { value = Both (t5, t6); _ }); _ })
                  ; _
                  } )
          ; _
          } ) ->
      Map6
        { f = (fun t1 t2 t3 t4 t5 t6 -> t1, (t2, (t3, (t4, (t5, t6)))))
        ; t1
        ; t2
        ; t3
        ; t4
        ; t5
        ; t6
        }
    | Both
        ( t1
        , { value = Both (t2, { value = Both (t3, { value = Both (t4, t5); _ }); _ }); _ }
        ) ->
      Map5 { f = (fun t1 t2 t3 t4 t5 -> t1, (t2, (t3, (t4, t5)))); t1; t2; t3; t4; t5 }
    | Both (t1, { value = Both (t2, { value = Both (t3, t4); _ }); _ }) ->
      Map4 { f = (fun t1 t2 t3 t4 -> t1, (t2, (t3, t4))); t1; t2; t3; t4 }
    | Both (t1, { value = Both (t2, t3); _ }) ->
      Map3 { f = (fun t1 t2 t3 -> t1, (t2, t3)); t1; t2; t3 }
    | v -> v
  in
  { Value.value; here; id }
;;

module Types = struct
  module Down = Unit
  module Acc = Unit
  module Up = Fix_transform.Unit
end

module Flatten_values (Recurse : Fix_transform.Recurse with module Types := Types) =
struct
  let transform_v () () v =
    let out = value_map v in
    Recurse.on_value () () `Skipping_over out
  ;;

  let transform_c () () c = Recurse.on_computation () () `Skipping_over c
end

open Fix_transform.Make (Types) (Flatten_values)

let flatten_values c =
  let (), (), r = Trampoline.run (transform_c () () c) in
  r
;;
