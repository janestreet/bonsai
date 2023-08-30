include
  [%css
  stylesheet
    {|
  .column_header {
    white-space: pre;
    cursor: pointer;
  }

  /* The default value for the [overflow-anchor] CSS property is [auto], which
     permits the browser to scroll the page in order to minimize content shifts.
     This interacts poorly with the PRT because our virtual-dom diff-and-patch
     algorithm often removes and re-inserts elements. To fix this, we disable
     overflow-anchor for all elements that contain a partial render table. */
  :has(.partial_render_table_container) {
    overflow-anchor: none;
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
