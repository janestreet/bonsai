open! Core
open! Import

module Direction = struct
  type t =
    | Top
    | Right
    | Bottom
    | Left
end

module Style =
  [%css.hash_variables
    stylesheet
      {|
  .tooltip_container {
    --dist: 0.3em;

    position: relative;
    display: inline-flex;
    justify-content: center;

    text-decoration: underline;
    text-decoration-style: dotted;
    text-underline-offset: 0.15em;
  }

  .tooltip {
      /* one day "top-layer" will save us https://fullscreen.spec.whatwg.org/#top-layer */
      z-index: 9001
  }

  .tooltip_container>.tooltip {
      display:none;
  }

  .tooltip_container:hover>.tooltip {
    display:block;
    position: absolute;

    padding: 0.1em 0.3em;
    max-width: max(300px, 100%);
    width: max-content;

    border: 1px solid var(--border);
    border-radius: 2px;
    background: var(--bg);
    color: var(--fg);
  }

  .tooltip_container.top,
  .tooltip_container.bottom {
    flex-direction: row;
  }

  .tooltip_container.left,
  .tooltip_container.right {
    flex-direction: column;
  }

  .tooltip_container.top>.tooltip {
    transform: translateY(-100%);
  }

  .tooltip_container.bottom>.tooltip {
    top: 100%;
  }

  .tooltip_container.left {
      padding-left: var(--dist);
      /* You can't just negate a var, so subtract it from 0 */
      margin-left: calc(0 - var(--dist));
  }

  .tooltip_container.right {
    padding-right: var(--dist);
    margin-right: calc(0 - var(--dist));
  }

  .tooltip_container.left>.tooltip {
    transform: translateX(calc(-100% - var(--dist)));
  }

  .tooltip_container.right>.tooltip {
    left: 100%;
  }
|}]

let make
      (constants : Constants.t)
      ~container_attr
      ~tooltip_attr
      ~direction
      ~tipped
      ~tooltip
  =
  let dir_class =
    match (direction : Direction.t) with
    | Top -> Style.top
    | Bottom -> Style.bottom
    | Left -> Style.left
    | Right -> Style.right
  in
  let tooltip =
    let vars =
      Style.Variables.set
        ~bg:(Color.to_string_css constants.extreme.background)
        ~fg:(Color.to_string_css constants.extreme.foreground)
        ~border:(Color.to_string_css constants.extreme_primary_border)
        ()
    in
    Vdom.Node.div ~attr:(Vdom.Attr.many [ Style.tooltip; tooltip_attr; vars ]) [ tooltip ]
  in
  Vdom.Node.span
    ~attr:(Vdom.Attr.many [ container_attr; Style.tooltip_container; dir_class ])
    [ tipped; tooltip ]
;;
