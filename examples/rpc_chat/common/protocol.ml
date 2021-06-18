open! Core
open! Async_rpc_kernel

module Message_stream = struct
  let t =
    Rpc.Pipe_rpc.create
      ~name:"messages"
      ~version:0
      ~bin_query:[%bin_type_class: unit]
      ~bin_response:[%bin_type_class: Message.t]
      ~bin_error:[%bin_type_class: unit]
      ()
  ;;
end

module Messages_request = struct
  let t =
    Rpc.Rpc.create
      ~name:"get_messages"
      ~version:0
      ~bin_query:[%bin_type_class: Room.t]
      ~bin_response:[%bin_type_class: Message.t List.t]
  ;;
end

module Send_message = struct
  let t =
    Rpc.Rpc.create
      ~name:"send_message"
      ~version:0
      ~bin_query:[%bin_type_class: Message.t]
      ~bin_response:[%bin_type_class: unit Or_error.t]
  ;;
end

module Create_room = struct
  let t =
    Rpc.Rpc.create
      ~name:"create_room"
      ~version:0
      ~bin_query:[%bin_type_class: Room.t]
      ~bin_response:[%bin_type_class: unit Or_error.t]
  ;;
end

module List_rooms = struct
  let t =
    Rpc.Rpc.create
      ~name:"list_rooms"
      ~version:0
      ~bin_query:[%bin_type_class: unit]
      ~bin_response:[%bin_type_class: Room.t List.t]
  ;;
end
