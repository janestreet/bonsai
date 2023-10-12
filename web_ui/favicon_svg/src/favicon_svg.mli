open Core

type t

(** SVG favicon displaying given unicode string, scaled to fit the space.

    For best effects, use a single character (there is plenty to choose from in unicode,
    e.g. emojis!) or a short string.

    [font_size] is relative to the size of the icon. [0.9] is a reasonable starting point,
    but you might need to tune it.

    [font_color] only makes sense for text or b&w emojis / symbols. *)
val of_unicode
  :  font_size:Percent.t
  -> ?background_color:Css_gen.Color.t
  -> ?font_color:Css_gen.Color.t
  -> ?pos_x:Percent.t
  -> ?pos_y:Percent.t
  -> string
  -> t

(** Use [to_svg_file_content] to save to file, or as [~contents] argument to
    [Cohttp_static_handler.What_to_serve.Embedded] *)
val to_svg_file_content : t -> string

(** Use [to_embedded_url] to get a data URL containing the svg. It can be used e.g. with
    [Cohttp_static_handler.External] *)
val to_embedded_url : t -> Uri.t
