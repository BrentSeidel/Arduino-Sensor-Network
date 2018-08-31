//
// Requests a value from the server using AJAX.
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

  document.getElementById("count").innerHTML = count + " requests serviced<br>" +
                                               tasks + " current active tasks";
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
  xhttp = new XMLHttpRequest();

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
  xhttp = new XMLHttpRequest();

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


