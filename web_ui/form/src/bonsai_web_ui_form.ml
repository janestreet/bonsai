open! Core

(** [With_manual_view] forms are now the recommended way to build forms in Bonsai. You get
    full control over how you'd like to combine them. Historically, [With_automatic_view]
    was the default, but they are quite restrictive and hard to customize, so we recommend
    choosing [With_manual_view] for new forms. *)
module With_manual_view = struct
  include Form_manual

  module Elements = struct
    include Elements_manual
    include Typed_elements_manual
  end

  module Typed = Typed_manual

  module Private = struct
    include Elements_manual.Private
  end
end

(** [With_automatic_view] forms have their views composed in an opinionated way. This used
    to be the default and recommended way to build forms in Bonsai, but now people should
    prefer [With_manual_view] where possible. *)
module With_automatic_view = struct
  include Form_automatic
  module Elements = Elements_automatic
  module Typed = Typed_automatic
end
