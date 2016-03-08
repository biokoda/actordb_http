// requires request and msgpack5
// npm install request
// npm install msgpack5

var request = require('request');
var username = 'root';
var password = 'rootpass';

var msgpack = require('msgpack5')() // namespace our extensions
  , encode  = msgpack.encode
  , decode  = msgpack.decode

var options = {
  uri:'http://localhost:33380/v1/_db/',
  method:"GET",
  encoding:null,
  headers:{
    'Accept': 'application/x-msgpack',
    'Content-Type': 'application/x-msgpack',
    'Authorization': "Basic " + new Buffer(username + ":" + password).toString("base64")
    }
}

request(options,function(err, msg, response){
  console.log(decode(response));
});
