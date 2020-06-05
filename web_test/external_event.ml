open! Core_kernel

include Bonsai_web.Vdom.Event.Define (struct
    module Action = String

    let handle str = printf "External event: %s\n" str
  end)
