open! Core

include Bonsai_web.Vdom.Effect.Define (struct
    module Action = String

    let handle str = printf "External event: %s\n" str
  end)
