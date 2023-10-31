open! Core
include Form

module Elements = struct
  include Elements
  include Typed_elements
end

module Typed = Typed

module Private = struct
  include Elements.Private
end
