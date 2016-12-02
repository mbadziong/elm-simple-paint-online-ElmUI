var express = require('express');
var app = express();
var expressWS = require('express-ws')(app);

var portNumber = 1234;
var lines = [];

app.listen(portNumber, function () {
  console.log(`Listening on port ${portNumber}`);
});

var aWss = expressWS.getWss('/test');

app.ws('/test', function (websocket, request) {
  console.log('A client connected!');
  if (lines && lines.length > 0) {
    websocket.send(JSON.stringify(lines));
  }

  websocket.on('message', function (message) {
    var msg = JSON.parse(message);

    if (msg.clear !== undefined) {
      // clear persisted lines
      lines = [];
    } else {
      // it's line
      lines.push(msg);
    }

    aWss.clients.forEach(function (client) {
      client.send(JSON.stringify(lines));
    });
  });
});