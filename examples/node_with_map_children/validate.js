globalThis.domNodeColorValidator = function () {
  try {
    var results_container = globalThis.document.querySelector("#results");
    var chdn = Array.from(results_container.childNodes);

    var length = chdn[0].childNodes[0].childNodes.length;

    // for each column
    for (var i = 0; i < chdn.length; i++) {
      var l = chdn[0].childNodes[0].childNodes.length;
      if (l !== length) {
        throw new Error("column 0 and " + i + " disagree about how many child elements should be in the dom");
      }
    }

    // for each row 
    for (var k = 0; k < length; k++) {
      var as_string = undefined;
      // for each column
      for (var i = 0; i < chdn.length; i++) {
        var e = chdn[0].childNodes[0].childNodes[k];
        var e_html = e.outerHTML;
        if (typeof as_string === "undefined") {
          as_string = e_html;
        } else if (as_string !== e_html) {
          throw new Error("cell at column 0 and " + i + " disagree about the contents of row " + k + " " + as_string + " " + e_html);
        }
      }
    }

  } catch (e) {
    globalThis.console.error(e);
    return 0;
  }
  return 1;
}
