var system = require('system');
var page   = require('webpage').create();
var url    = system.args[1];
//var dist = system.args[2];

page.open(url, function() {
  page.evaluate(function() {                                                                                                                                                                                       

  });
  setTimeout(function() {
      console.log(page.content);
      phantom.exit();
  },3000);
});

