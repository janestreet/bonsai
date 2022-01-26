open! Core

include
  [%css.raw
    {|
  * {
    box-sizing: border-box;
    font-family: sans-serif;
  }

  .pre_for_display {
      height: 50px;
   }

  .primary {
      background-color: rgb(30,30,30);
      color: rgb(240, 240, 240);
  }

  .resizable_using_css {
    background-color: rgb(30,30,30);
    color: rgb(240, 240, 240);
    resize: both;
    overflow: auto;
    width: 200px;
    height: 100px;
    border: 2px solid var(--js-form-unfocused-color);
  }

  .visibility_child {
    height: 2000px;
    width: 2000px;
  }

  .visibility_parent {
    height: 200px;
    width: 200px;
    overflow: scroll;
  }

  .resizer {
    height: 100%;
    width: 5px;
    display: inline-block;
    margin-left: 2px;
    background-color: rgb(30, 30, 30);
    right: 0px;
    top: 0px;
    position: absolute;
    cursor: col-resize;
  }

  .resizable_using_resizer {
    width: 300px;
    border: 2px solid rgb(30, 30, 30);
    position: relative;
    overflow: hidden;
  }
|}]
