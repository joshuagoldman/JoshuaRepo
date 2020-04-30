const fs = require('fs');

class ExcelToJson {
  constructor(filePath){
    this.parseExcel = function(sheetName) {
      
      var XLSX = require("XLSX");
      var workbook = XLSX.readFile(filePath);

      var XL_row_object = XLSX.utils.sheet_to_json(workbook.Sheets[sheetName], {defval:""});
      console.log(XL_row_object);
      var json_object = JSON.stringify(XL_row_object);

      fs.appendFile('excelToJson/test.txt', json_object, function (err) {
        if (err) throw err;
        console.log('Saved!');
      });

    };
  };
};

var xl = new ExcelToJson("C:\\Users\\egoljos\\RCO_List_AX.xlsx");
xl.parseExcel("Sheet1");