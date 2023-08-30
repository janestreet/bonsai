(** An API that allows one to read files from the client's local disk.

    A value of [type t] can be created in two ways:

    1. Using the companion library [Bonsai_web_ui_file_from_web_file], which uses the
    Web File API to drive [t]. Note that the Web File API does not permit the reading of
    arbitrary files on the client computer's disk. Instead, one typically uses a file
    selector form to allow the user to specify a file to be read. See
    [Bonsai_web_ui_form.Elements.File_picker] for a convenient wrapper.

    2. For tests, one can also create a [t] which is driven manually using the
    [For_testing] module.
*)

open Core

type t [@@deriving sexp_of]

val filename : t -> string

(** Get the contents of the file. For an advanced API that includes e.g. progress on
    loading the file, and the ability to abort in-progress reads, see [read] below. *)
val contents : t -> Bigstring.t Or_error.t Ui_effect.t

module Progress : sig
  type t =
    { loaded : int (** How many bytes have been loaded so far? *)
    ; total : int (** How many bytes are there to read in total? *)
    }
  [@@deriving compare, equal, sexp]

  val to_percentage : t -> Percent.t
end

module Read_error : sig
  type t =
    | Aborted
    | Error of Error.t
  [@@deriving compare, equal, sexp]
end

module File_read : sig
  (** An in-progress file read. *)
  type t

  (** Gets the result of the read. Will only return [Error Aborted] if you call [abort]. *)
  val result : t -> (Bigstring.t, Read_error.t) Result.t Ui_effect.t

  val abort : t -> unit Ui_effect.t
end

val read : ?on_progress:(Progress.t -> unit Ui_effect.t) -> t -> File_read.t Ui_effect.t

module Read_on_change : sig
  (** Functions that take bonsai values that contain [t], and kicking off reads whenever
      those values change.

      The arguments here are chosen to match the results from
      [Bonsai_web_ui_form.Elements.File_picker]. So you can do things like:

      {[
        let%sub file_picker = Bonsai_web_ui_form.Elements.File_select.single () in
        let%sub file_from_form =
          let%arr file_picker = file_picker in
          Bonsai_web_ui_form.value file_picker |> Or_error.ok
        in
        let%sub result = Bonsai_web_ui_file.Read_on_change.create_single_opt file_from_form in
        match%sub result with
        | None -> Bonsai.const None
        | Some (filename, (Bonsai_web_ui_file.Read_on_change.Status.Starting | In_progress _))
          ->
          let%arr filename = filename in
          Some (filename, "file still loading")
        | Some (filename, Complete (Error e)) ->
          let%arr e = e
          and filename = filename in
          Some (filename, Error.to_string_hum e)
        | Some (filename, Complete (Ok contents)) ->
          let%arr filename = filename
          and contents = contents in
          Some (filename, contents)
      ]}

      NOTE: these computations are not safe for use in Tangle as internally they require a
      model which cannot be [of_sexp]'d.
  *)

  module Status : sig
    type t =
      | Starting
          (** The file read has been kicked off but no progress has been received yet *)
      | In_progress of Progress.t
      | Complete of Bigstring.t Or_error.t
    [@@deriving sexp_of]
  end

  val create_single : t Bonsai.Value.t -> (Filename.t * Status.t) Bonsai.Computation.t

  val create_single_opt
    :  t option Bonsai.Value.t
    -> (Filename.t * Status.t) option Bonsai.Computation.t

  val create_multiple
    :  t Filename.Map.t Bonsai.Value.t
    -> Status.t Filename.Map.t Bonsai.Computation.t
end

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

    (** Describes whether a read is currently ongoing for the stream in question. *)
    val read_status : t -> [ `Aborted | `Not_reading | `Reading ]
  end

  val create : Test_data.t -> t
end

module Expert : sig
  type file_read =
    { result : (Bigstring.t, Read_error.t) Result.t Ui_effect.t
    ; abort : unit Ui_effect.t
    }

  val create
    :  read:((Progress.t -> unit Ui_effect.t) -> file_read Ui_effect.t)
    -> filename:string
    -> t
end
