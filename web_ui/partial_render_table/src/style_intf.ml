include
  [%css.raw
    {|
  .column_header {
    white-space: pre;
    cursor: pointer;
  }

  .partial_render_table_container {
    width: max-content;
    position: relative;
  }

  .partial_render_table_body {
    position: relative;
  }

  .header_label {
    text-align: center;
    user-select: none;
    font-weight: bold;
  }

  .leaf_header {
    resize: horizontal;
    overflow: hidden;
    box-sizing: border-box;
  }

  .partial_render_table_header {
    position: sticky;
    top: 0px;
    z-index: 99;
  }

  .cell {
    box-sizing: border-box;
    overflow:hidden;
    display:inline-block;
    contain: strict;
  }
  |}]
