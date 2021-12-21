open! Core
include Bonsai_web
include Bonsai.Let_syntax
module Form = Bonsai_web_ui_form

module Card_like =
  [%css.raw
    {|
.class_ {
  border-radius: 8px;
  background-color: white;
  box-shadow: rgb(0 0 0 / 10%) 0px 4px 6px -1px, rgb(0 0 0 / 6%) 0px 2px 4px -1px;
  border: none;
  padding: 8px;
}
|}]
