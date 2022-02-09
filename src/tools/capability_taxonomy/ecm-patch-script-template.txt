/** Google App script to modify an ECM matrix */

function groupIt(str1) {
  var spreadsheet = SpreadsheetApp.getActive();
  spreadsheet.getRange(str1).activate()
  .shiftRowGroupDepth(1);
};

function ungroupIt(str1) {
  var spreadsheet = SpreadsheetApp.getActive();
  spreadsheet.getRange(str1).activate()
  .shiftRowGroupDepth(-1);
};

function main() {
  @@BODY
}

