var request = require('request');
var username = 'root';
var password = 'rootpass';

var options = {
  uri:'http://localhost:33380/v1/_db/',
  method:"GET",
  json:true,
  headers:{
    'Accept': 'application/json',
    'Content-Type': 'application/json',
      'Authorization': "Basic " + new Buffer(username + ":" + password).toString("base64")
    }
}
request(options,function(err, msg, response){
  console.log(response);
});
