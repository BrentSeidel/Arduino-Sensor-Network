"use strict";
//
// Call this routine when the page is finished loading.
//
function page_loaded()
{
  debug_req(-1);
  loadCounter();
}
//
// Requests counter values from the server using AJAX.
//
function loadCounter()
{
  var xhttp = new XMLHttpRequest();

  xhttp.onreadystatechange = function() {
    if (this.readyState == 4 && this.status == 200) {
      displayCounter(this);
    }
  };
  xhttp.open("GET", "/xml/Counter", true);
  xhttp.send();
}
//
// Displays the value requested in the previous function in the "counter" element.
//
function displayCounter(xml)
{
  var xmlDoc = xml.responseXML;
  var count = xmlDoc.getElementsByTagName("counter")[0].childNodes[0].nodeValue;
  var tasks = xmlDoc.getElementsByTagName("tasks")[0].childNodes[0].nodeValue;
  var activity = xmlDoc.getElementsByTagName("rs485")[0].childNodes[0].nodeValue;
  var text;

  if (count > 1)
  {
    text = count + " requests serviced<br/>";
  }
  else
  {
    text = count + " request serviced<br/>";
  }
  if (tasks > 1)
  {
    text += tasks + " current active tasks" ;
  }
  else
  {
    text += tasks + " current active task" ;
  }
  text += "<br/>RS-485 Activity counter " + activity;

  document.getElementById("count").innerHTML = text;
}
//
// AJAX call to send a command passed as a string
//
function sendCommand(cmd)
{
  var xhttp = new XMLHttpRequest();

  xhttp.onreadystatechange = function() {
    if (this.readyState == 4 && this.status == 200) {
      cmdResp(this);
    }
  };
  xhttp.open("GET", "/xml/Command?command=" + cmd, true);
  xhttp.send();
}
//
// AJAX call to send a command contained in a field
//
function sendCmd()
{
  var xhttp = new XMLHttpRequest();
  var cmd = document.getElementById("command").value;

  xhttp.onreadystatechange = function() {
    if (this.readyState == 4 && this.status == 200) {
      cmdResp(this);
    }
  };
  xhttp.open("GET", "/xml/Command?command=" + cmd, true);
  xhttp.send();
}
//
// AJAX response to display the command response.
//
function cmdResp(xml)
{
  var xmlDoc = xml.responseXML;
  var cmd = xmlDoc.getElementsByTagName("command");
  var err = xmlDoc.getElementsByTagName("error");

  if (cmd.length > err.length)
  {
    document.getElementById("cmdResp").innerHTML = "Got command \"" +
                                       cmd[0].childNodes[0].nodeValue + "\"";
  }
  else
  {
    document.getElementById("cmdResp").innerHTML = "Error message \"" +
                                        error[0].childNodes[0].nodeValue + "\"";
  }
}
//
// Load the device names.  First need to get the count.  Then when that comes
// back, loop to request each name.
//
function loadNames()
{
  var xhttp = new XMLHttpRequest();

  xhttp.onreadystatechange = function() {
    if (this.readyState == 4 && this.status == 200) {
      reqNames(this);
    }
  };
  xhttp.open("GET", "/xml/Devices", true);
  xhttp.send();
}
//
// Request names
//
function reqNames(xml)
{
  var xmlDoc = xml.responseXML;
  var x = xmlDoc.getElementsByTagName("length")[0].childNodes[0].nodeValue;
  var device;
  var table;

  table = "<table><tr><th>ID</th><th>Name</th></tr>";
  for (device = 0; device < x; device++)
  {
    table += "<tr><td>" + device + "</td><td id=\"dev-" + device + "\"></td></tr>";
    sendDeviceRequest(device)
  }
  table += "</table>";
  document.getElementById("devices").innerHTML = table;
}
//
function sendDeviceRequest(dev)
{
  var xhttp = new XMLHttpRequest();

  xhttp.onreadystatechange = function() {
    if (this.readyState == 4 && this.status == 200) {
      displayName(this, dev);
    }
  }
  xhttp.open("GET", "/xml/Name?device=" + dev, true);
  xhttp.send();
}
//
function displayName(xml, device)
{
  var xmlDoc = xml.responseXML;
  var error = xmlDoc.getElementsByTagName("error");
  var name = xmlDoc.getElementsByTagName("name");
  var text;

  if (error.length > 0)
  {
    text = error[0].childNodes[0].nodeValue;
  }
  else
  {
    text = "<p onclick=\"reqDevData(" + device + ")\">" +
           name[0].childNodes[0].nodeValue + "</p>";
  }
  document.getElementById("dev-" + device).innerHTML = text;
}
//
// Request and present some device data
//
function reqDevData(dev)
{
  var xhttp = new XMLHttpRequest();

  xhttp.onreadystatechange = function() {
    if (this.readyState == 4 && this.status == 200) {
      dispDevData(this);
    }
  }
  xhttp.open("GET", "/xml/DevData?device=" + dev, true);
  xhttp.send();
}
//
function dispDevData(xml)
{
  var xmlDoc = xml.responseXML;
  var error = xmlDoc.getElementsByTagName("error");
  var name = xmlDoc.getElementsByTagName("name");
  var temp;
  var text;

  if (error.length > 0)
  {
    text = error[0].childNodes[0].nodeValue;
  }
  else
  {
    text = "<h1>" + name[0].childNodes[0].nodeValue + "</h1>";
  }
    document.getElementById("DevData").innerHTML = text;
}
//
// Send request for debugging.  This can also be called with no parameters to
// just return the status of the debug flags.
//
function debug_req(index)
{
  var xhttp = new XMLHttpRequest();
  var item;
  var state;
  var req = "/xml/Debug";

  if (index >= 0)
  {
    item = ["rs485.char", "rs485.msg", "http.msg", "http.head", "web.dbg"][index];
    req += "?" + item + "=";
    state = document.getElementById(item).checked;
    if (state == true)
    {
      req += "True";
    }
    else
    {
      req += "False"
    }
  }
  xhttp.onreadystatechange = function() {
    if (this.readyState == 4 && this.status == 200) {
      debug_resp(this);
    }
  }
  xhttp.open("GET", req, true);
  xhttp.send();
}
//
// Display response with debugging information
//
function debug_resp(xml)
{
  var xmlDoc = xml.responseXML;
  var item;

  check_state(xmlDoc, "rs485.char");
  check_state(xmlDoc, "rs485.msg");
  check_state(xmlDoc, "http.msg");
  check_state(xmlDoc, "http.head");
  check_state(xmlDoc, "web.dbg");
}
//
// Helper function to update checkbox state
//
function check_state(xml, name)
{
  var item = xml.getElementsByTagName(name);

  if (item.length > 0)
  {
    if (item[0].childNodes[0].nodeValue == "TRUE")
    {
      document.getElementById(name).checked = true;
    }
    else
    {
      document.getElementById(name).checked = false;
    }
  }
}

