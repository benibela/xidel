function encodeForm(form){
  var res = "";
  var x = form.getElementsByTagName("textarea");
  for (var i=0;i<x.length;i++) 
    res += "&" + encodeURIComponent(x[i].name) + "=" + encodeURIComponent(x[i].value);
  
  var x = form.getElementsByTagName("select");
  for (var i=0;i<x.length;i++) 
    res += "&" + encodeURIComponent(x[i].name) + "=" + encodeURIComponent(x[i].value);

  x = form.getElementsByTagName("input");
  for (var i=0;i<x.length;i++) 
    if (x[i].type == "text") res += "&" + encodeURIComponent(x[i].name) + "=" + encodeURIComponent(x[i].value);
    else if (x[i].type == "checkbox" || x[i].type == "radio") 
      if (x[i].checked) res += "&" + encodeURIComponent(x[i].name) + "=" + encodeURIComponent(x[i].value);
      
  return res;
}

var lastFormEncoding = "";

function sendForm(form, callback){
    //var fd = new FormData(form); 
    //fd.append("raw", "true");
    var  req = new XMLHttpRequest(); 
    req.open("POST", form.action);
    req.setRequestHeader("Content-Type", "application/x-www-form-urlencoded");
    if (callback) 
      req.onreadystatechange = function(){
        if (req.readyState!=4) return; 
        if (req.status==200) callback(req.responseText, req); /*else alert("request failed: "+message+", "+data+"\n => "+req.status+" "+req.responseText);  */ 
      }
    lastFormEncoding = encodeForm(form);
    req.send("raw=true"+lastFormEncoding);
}
var lastCallTime;
var changed = false;
function updateNow(calltime) { 
  if (lastCallTime > calltime) return;  //alert(calltime);
  sendForm(document.getElementsByTagName("form")[0], function(answer){
    document.getElementById("result").value = answer;
    document.getElementById("permalink").href = "http://videlibri.sourceforge.net/cgi-bin/xidelcgi?"+lastFormEncoding;
  });
}
function update(){
  if (document.getElementsByName("no-auto-update")[0].checked) return;
  (function (boundTime){
    lastCallTime = boundTime;
    setTimeout(function () {updateNow(boundTime); }, 500); 
  }) (new Date().getTime());
}
function changeexample(example){
  if (changed) return;
  document.getElementsByName("extract")[0].value = example;
}
function init(){
  var form = document.getElementsByTagName("form")[0];
  var x = form.getElementsByTagName("input");
  for (var i=0;i<x.length;i++) x[i].onchange = update;
  var x = form.getElementsByTagName("select");
  for (var i=0;i<x.length;i++) x[i].onchange = update;
  var x = form.getElementsByTagName("textarea");
  for (var i=0;i<x.length;i++) 
    x[i].onkeyup = (function(_i){ return function(event){
      var key = event.keyCode ? event.keyCode : event.charCode;
      if (!(key >= 16 && key <= 18 /*ShiftCtrlAlt*/ ) && !(key >= 33 && key <= 45 /* e.g. arrows */)) {
        if (_i == 1) changed = true;
        update();
      }}})(i);
};

