var request = require('request');
var username = 'root';
var password = 'rootpass';

var options = {
  uri:'http://localhost:33380/v1/q/exec',
  method:"POST",
  json:true,
  headers:{
    'Accept': 'application/json',
    'Content-Type': 'application/json',
      'Authorization': "Basic " + new Buffer(username + ":" + password).toString("base64")
    },
  body: { "q": "ACTOR type1(123); SELECT * FROM tab1;"}
}
request(options,function(err, msg, response){
  console.log(response);
});
