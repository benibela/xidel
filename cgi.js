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
var lastQueryEditMode = "xpath";
function updateNow(calltime, codemirror) { 
  if (lastCallTime > calltime) return;  //alert(calltime);
//  if (codemirror && codemirror.save) { alert("save test"); codemirror.save();} //did not work. Another update overriding this?
  sendForm(document.getElementsByTagName("form")[0], function(answer, request){
    if (window.outputMirror) window.outputMirror.setValue(answer);
    else  document.getElementById("result").value = answer;
    document.getElementById("permalink").href = "http://videlibri.sourceforge.net/cgi-bin/xidelcgi?"+lastFormEncoding;
    document.getElementById("rawpermalink").href = "http://videlibri.sourceforge.net/cgi-bin/xidelcgi?raw=true"+lastFormEncoding;
    lastQueryEditMode = request.getResponseHeader("Xidel-Detected-Extraction-Kind");
    setRealEditMode();
  });
}
function update(codemirror){
  if (document.getElementsByName("no-auto-update")[0].checked) return;
  (function (boundTime){
    lastCallTime = boundTime;
    setTimeout(function () {updateNow(boundTime, codemirror); }, 500); 
  }) (new Date().getTime());
}
var editMode = "auto";
var realEditMode = "auto";
function changeexample(examplename, example){
  examplename = examplename.toLowerCase();
  if (examplename.indexOf("template") >= 0) editMode = "template";
  else if (examplename.indexOf("xpath") >= 0) editMode = "xpath";
  else if (examplename.indexOf("xquery") >= 0) editMode = "xquery";
  else if (examplename.indexOf("css") >= 0) editMode = "css";
  else editMode="auto";
  setRealEditMode();

  if (changed) return;
  document.getElementsByName("extract")[0].value = example;
  if (window.extractCodeMirror) 
    window.extractCodeMirror.setValue(example);
  changed = false;
}
function setRealEditMode(){
  if (editMode == "auto") realEditMode = lastQueryEditMode;
  else realEditMode = editMode;
  
  function addjsoniq(){return (document.getElementsByName("no-json")[0].checked ? "" : "+jsoniq")}
  if (window.extractCodeMirror) {
    if (realEditMode == "auto" || realEditMode == "") {
      realEditMode = "xquery";
      var temp = window.extractCodeMirror.getLine(0);
      var i = 0; while (temp == "" && i < window.extractCodeMirror.lineCount()) { i+=1; temp = window.extractCodeMirror.getLine(i); }
      for (i = 0; i < temp.length; i++) {
        if (temp.charAt(i) != " " && temp.charAt(i) != "\t") {
          if (temp.charAt(i) == "<") realEditMode = "template"; 
          else if (temp.charAt(i) == "#" || temp.charAt(i) == ".") realEditMode = "css"; 
          else realEditMode = "xquery";
          break;
        }
      }
    }
    if (realEditMode == "template") window.extractCodeMirror.setOption("mode", "xquery" + addjsoniq())
    else if (realEditMode == "css") window.extractCodeMirror.setOption("mode", "css");
    else window.extractCodeMirror.setOption("mode", realEditMode + addjsoniq());
  }
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
      
  var compatSelect = document.getElementsByName("compatibility")[0];
  compatSelect.onchange = function(){
    if (compatSelect.selectedIndex < 3) {
      var temp = compatibilityOn[compatSelect.selectedIndex].split(";");
      for (var i=0;i<temp.length;i++) $("input[name="+temp[i]+"]").prop('checked', true);
      var temp = compatibilityOff[compatSelect.selectedIndex].split(";");
      for (var i=0;i<temp.length;i++) $("input[name="+temp[i]+"]").prop('checked', false);
      var temp = compatibilityChange[compatSelect.selectedIndex].split(";");
      for (var i=0;i<temp.length;i++) $("select[name="+temp[i].split("=")[0]+"]").val(temp[i].split("=")[1]);
      $("#compatibilityOptions").hide();
    } else $("#compatibilityOptions").show();
    update();
  }
};
function transformTextArea(x, autoupdate){
  var myCodeMirror = CodeMirror.fromTextArea(x, {
    lineNumbers: true,
  });

  myCodeMirror.xxxThisIsACodeMirror = true;
  myCodeMirror.setSize($(x).width(), $(x).height());

 $('.CodeMirror').resizable({
    resize: function() {
      editor.setSize($(this).width(), $(this).height());
    }
  });
  return myCodeMirror;
}
var codeMirrorsWereActivated = false;
    toggleInitialized = false;
    
function activateCodeMirrors(){
  var compatSelect = document.getElementsByName("compatibility")[0];
  if (compatSelect.selectedIndex < 3) $("#compatibilityOptions").hide();

  if (window.extractCodeMirror) return;
  var toggle = document.getElementsByName("no-highlighting")[0];
  if (!toggleInitialized) {
    toggle.addEventListener("change", function(){
      if (toggle.checked) deactivateCodeMirrors();
      else activateCodeMirrors();
    });  
    //toggle.parentNode.style.display = "inline";
    toggleInitialized = true;
  }
  if (toggle.checked) return;
  
  window.extractCodeMirror =  transformTextArea(document.getElementsByName("extract")[0]);
  window.extractCodeMirror.on("change", function(){ changed=true; window.extractCodeMirror.save(); update(window.extractCodeMirror);});
  if (!codeMirrorsWereActivated)
    document.getElementsByName("no-json")[0].addEventListener("change", setRealEditMode);
  setRealEditMode();

  window.inputMirror =  transformTextArea(document.getElementsByName("data")[0], true);
  window.inputMirror.on("change", function(){window.inputMirror.save(); update(window.inputMirror);});
  function inputChanged(){
    var inputformat = document.getElementsByName("input-format")[0].value;
    if (inputformat == "auto") {
      if (inputMirror.getLine(0).indexOf("<html>") >= 0 || inputMirror.getLine(1).indexOf("<html>") >= 0)
        inputformat = "html";
       else
        inputformat = "xml";
    }
    if (inputformat == "html")
      window.inputMirror.setOption("mode", "htmlmixed");
    else
      window.inputMirror.setOption("mode", "xml");
  }

  inputChanged();
  window.inputMirror.on("change", inputChanged);
  if (!codeMirrorsWereActivated)
    document.getElementsByName("input-format")[0].addEventListener("change", inputChanged);



  window.outputMirror =  transformTextArea(document.getElementById("result"));
  function outputChanged(){
     var outputformat = document.getElementsByName("output-format")[0].value;
     if (outputformat == "html") window.outputMirror.setOption("mode", "htmlmixed");
     else if (outputformat == "xml" || outputformat == "xml-wrapped") window.outputMirror.setOption("mode", "xml");
     else if (outputformat == "json-wrapped") window.outputMirror.setOption("mode", "javascript");
     else if (document.getElementsByName("printed-node-format")[0].value == "xml") window.outputMirror.setOption("mode", "xml");
     else if (document.getElementsByName("printed-node-format")[0].value == "html") window.outputMirror.setOption("mode", "htmlmixed");
     else window.outputMirror.setOption("mode", "none");
  }
  if (!codeMirrorsWereActivated) { 
    document.getElementsByName("output-format")[0].addEventListener("change", outputChanged);
    document.getElementsByName("printed-node-format")[0].addEventListener("change", outputChanged);
  }
  outputChanged();
  
  codeMirrorsWereActivated = true; 
}

function deactivateCodeMirrors(){
  if (!window.extractCodeMirror) return;
  window.extractCodeMirror.toTextArea();
  window.inputMirror.toTextArea();
  window.outputMirror.toTextArea();
  window.extractCodeMirror = null;
  window.inputMirror = null;
  window.outputMirror = null;
}
