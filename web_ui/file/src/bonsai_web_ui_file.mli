(** An API that allows one to read files from the client's local disk. Reading of
    arbitrary files is not created; typically one uses a widget using
    Bonsai_web_ui_form.Element.File_select to allow the user to select a file or files.
*)

open Core

type t [@@deriving sexp_of]

val filename : t -> string

(** Read the contents of the file. For more precise control, including progress updates,
    you can use [read] and the [File_read] API. *)
val contents : t -> string Or_error.t Bonsai.Effect.t

module File_read : sig
  (** A [File_read.t] represents an in-progress read of a file. *)
  type t

  module Progress : sig
    type t =
      { loaded : int (** How many bytes have been loaded so far? *)
      ; total : int (** How many bytes are there to read in total? *)
      }
    [@@deriving compare, sexp_of]

    val to_percentage : t -> Percent.t
  end

  module State : sig
    type t =
      | Contents of string
      | Loading of Progress.t option
      | Error of Error.t
    [@@deriving compare, equal, sexp]
  end

  val state : t -> State.t Bonsai.Value.t

  type error =
    | Aborted
    | Error of Error.t
  [@@deriving compare, sexp_of]

  (** Get the result of the read, or an error. Will only return [Aborted] if you call
      [abort]. *)
  val result : t -> (string, error) Result.t Bonsai.Effect.t

  (** Abort the file read. [result] will become determined with result [Aborted] *)
  val abort : t -> unit
end

val read
  :  ?on_state_change:
    (
      File_read.State.t
      -> Ui_event.t)
  -> t
  -> File_read.t

module For_testing : sig
  (** This module supports a test / simulation mode where file data can be fed in by you
      rather than coming from an actual file on disk. *)

  module Test_data : sig
    type t

    (** Create a [t] with the given [contents] *)
    val create_static : filename:string -> contents:string -> t

    (** Create a [t] that can be fed bytes continually using [feed_exn].

        [total_bytes] will be fed through to the [Progress] value in any reads. Note that
        we do nothing smart to close [t] when you have fed at least [total_bytes] bytes to
        [t]. *)
    val create_stream : filename:string -> total_bytes:int -> t

    (** If [t] was created using [create_stream], you can feed it more bytes. Raises if
        [t] was created using [create_static], or if [close] or [close_error] have been
        called. *)
    val feed_exn : t -> string -> unit

    (** Mark [t] as closed so that any file reads based on [t] will be marked complete.
        [close] is idempotent and does nothing if [t] was created using [create_static].
    *)
    val close : t -> unit

    (** Close [t] with an error to emulate an error when reading from the file. Does
        nothing if [close] has already been called or [t] was created using
        [create_static]. *)
    val close_error : t -> Error.t -> unit
  end

  val create : Test_data.t -> t
end

module Expert : sig
  val create : Js_of_ocaml.File.file Js_of_ocaml.Js.t -> t
end
