globalThis.domNodeColorValidator = function () {
  try {
    var results_container = globalThis.document.querySelector("#results");
    var chdn = Array.from(results_container.childNodes);

    var first_as_string = chdn[0].childNodes[0].outerHTML;

    // for each column
    for (var i = 1; i < chdn.length; i++) {
      var column_as_string = chdn[i].childNodes[0].outerHTML;
      if (first_as_string !== column_as_string) {
        throw new Error(
          "column 0 and " + i + " disagree about their contents:\n  " + 
          first_as_string + "\n  " + column_as_string);
      }
    }
  } catch (e) {
    globalThis.console.error(e);
    return 0;
  }
  return 1;
}
