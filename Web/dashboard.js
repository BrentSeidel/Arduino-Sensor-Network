//
// Global variables
//
var unitMetric = 1;  // Display in metric or imperial units
var deviceId = -1;   // Which device is currently being displayed
var dispTimer;       // Timer used for reloading the display
var numDevices = 0;  // Number of devices on RS-485 network
//
// Initializes the value and starts a timer to count up.  Apparently this can't
// just be called "animate".  Perhaps some browsers define it and some don't.
//
function startReload()
{
  loadNames();
  therm_timer = window.setInterval(reloadDisplay, 5000);
}
//
function reloadDisplay()
{
  loadNames();
  if (deviceId >= 0)
  {
    reqDevData(deviceId)
  }
}
//
// Set the value of the unitMetric flag
//
function setMetric(val)
{
  unitMetric = val;
  if (deviceId >= 0)
  {
    reqDevData(deviceId)
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

  if (x != numDevices)
  {
    table = "<table><tr><th>ID</th><th>Name</th></tr>";
    for (device = 0; device < x; device++)
    {
      table += "<tr><td>" + device + "</td><td id=\"dev-" + device + "\"></td></tr>";
      sendDeviceRequest(device)
    }
    table += "</table>";
    document.getElementById("devices").innerHTML = table;
    numDevices = x;
  }
  else // If the number of devices hasn't changed, don't rebuild the table.
  {
    for (device = 0; device < x; device++)
    {
      sendDeviceRequest(device)
    }
  }
}
//
function sendDeviceRequest(dev)
{
  xhttp = new XMLHttpRequest();

  xhttp.onreadystatechange = function()
  {
    if (this.readyState == 4 && this.status == 200)
    {
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

  deviceId = dev;
  xhttp.onreadystatechange = function()
  {
    if (this.readyState == 4 && this.status == 200)
    {
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
  var temp_c;
  var temp_f;
  var pressure;
  var humidity;
  var type;
  var value;
  var i, j;

  if (error.length > 0)
  {
    text = error[0].childNodes[0].nodeValue;
  }
  else
  {
    text = "<h1>" + name[0].childNodes[0].nodeValue + "</h1>";
  }
  //
  // Discretes
  //
  temp = xmlDoc.getElementsByTagName("discrete");
  if (temp.length > 0)
  {
    type = parseInt(xmlDoc.getElementsByTagName("disc_type")[0].childNodes[0].nodeValue);
    value = parseInt(xmlDoc.getElementsByTagName("disc_value")[0].childNodes[0].nodeValue);
    text += "<p>Discretes value is " + value.toString(16) + ".</p>";
    text += "<table>";
    for (i = 3; i >= 0; i--)
    {
      text += "<tr>";
      for (j = 7; j >= 0; j--)
      {
        if ((value & (2**(i*8 + j))) == 0)
        {
          text += "<td class=\"clear\">0</td>";
        }
        else
        {
          text += "<td class=\"set\">1</td>";
        }
      }
      text += "</tr>";
    }
    text += "</table>";
  }
  //
  // BME280
  //
  temp = xmlDoc.getElementsByTagName("bme280");
  if (temp.length > 0)
  {
    text += add_BME280(temp[0].childNodes);
  }
  //
  // CCS811
  //
  temp = xmlDoc.getElementsByTagName("ccs811");
  if (temp.length > 0)
  {
    text += add_CCS811(temp[0].childNodes);
  }
  //
  // TSL2561
  //
  temp = xmlDoc.getElementsByTagName("tsl2561");
  if (temp.length > 0)
  {
    text += add_TSL2561(temp[0].childNodes);
  }
  document.getElementById("DevData").innerHTML = text;
}

function add_BME280(nodeList)
{
  var x;
  var node;
  var text = "<table><tr><th>Temperature</th><th>Pressure</th><th>Humidity</th></tr>";
  var temp_c;
  var pressure;
  var humidity;
  var validity;
  var age;
  var td_class = "container";

  //
  // Yes, this is a "for-case" type structure.  It's done this way because it's
  // possible that the order of the nodes may change.
  //
  for (x = 0; x < nodeList.length; x++)
  {
    node = nodeList[x];
    switch (node.nodeName)
    {
      case "validity":
        validity = node.childNodes[0].nodeValue;
        break;
      case "aging":
        age = parseFloat(node.childNodes[0].nodeValue);
        break;
      case "bme280_temp_c":
        temp_c = parseFloat(node.childNodes[0].nodeValue);
        break;
      case "bme280_pressure_pa":
        pressure = node.childNodes[0].nodeValue;
        break;
      case "bme280_humidity":
        humidity = parseFloat(node.childNodes[0].nodeValue);
        break;
    }
  }
  if (validity = "DATA_VALID")
  {
    if (age >= 10)
    {
      td_class += " stale";
    }
    if (unitMetric == 1)
    {
      text += "<tr><td class=\"" + td_class + "\"><img src=\"/Thermometer?min=0&max=100&value=" +
              Math.round(temp_c) + "\"></img></td>";
      text += "<td class=\"" + td_class + "\"><img src=\"/Thermometer?min=90000&max=100000&value=" +
              Math.round(pressure) + "\"></img></td>";
      text += "<td class=\"" + td_class + "\"><img src=\"/Dial?min=0&max=100&value=" + Math.round(humidity) +
             "\"></img></td></tr>";
      text += "<tr><td class=\"" + td_class + "\">" + temp_c.toFixed(1) + "&deg;C</td>";
      text += "<td class=\"" + td_class + "\">" + Math.round(pressure) + "Pa</td>";
    }
    else
    {
      temp_f = (temp_c*9/5) + 32;
      pressure = pressure/3386.39;
      text += "<tr><td class=\"" + td_class + "\"><img src=\"/Thermometer?min=50&max=150&value=" +
              Math.round(temp_f) + "\"></img></td>";
      text += "<td class=\"" + td_class + "\"><img src=\"/Thermometer?min=2500&max=3500&value=" +
              Math.round(pressure*100.0) + "\"></img></td>";
      text += "<td class=\"" + td_class + "\"><img src=\"/Dial?min=0&max=100&value=" + Math.round(humidity) +
             "\"></img></td></tr>";
      text += "<tr><td class=\"" + td_class + "\">" + temp_f.toFixed(1) + "&deg;F</td>";
      text += "<td class=\"" + td_class + "\">" + pressure.toFixed(2) + "inHg</td>";
    }
    text += "<td class=\"" + td_class + "\">" + humidity.toFixed(1) + "%</td></tr>";
  }
  else
  {
    td_class += " error";
    text += "<tr><td class=\"" + td_class + "\"><img src=\"/Thermometer?min=hello\"></img></td>";
    text += "<td class=\"" + td_class + "\"><img src=\"/Thermometer?min=hello\"></img></td>";
    text += "<td class=\"" + td_class + "\"><img src=\"/Dial?min=hello\"></img></td></tr>";
    text += "<tr><td class=\"" + td_class + "\">" + validity + "&deg;F</td>";
    text += "<td class=\"" + td_class + "\">" + validity + "inHg</td>";
    text += "<td class=\"" + td_class + "\">" + validity + "%</td></tr>";
  }
  text += "</table>";
  return text;
}

function add_CCS811(nodeList)
{
  var x;
  var node;
  var text = "<table><tr><th>CO<sub>2</sub></th><th>TVOC</th></tr>";
  var val_eco2;
  var val_tvoc;
  var validity;
  var age;

  //
  // Yes, this is a "for-case" type structure.  It's done this way because it's
  // possible that the order of the nodes may change.
  //
  for (x = 0; x < nodeList.length; x++)
  {
    node = nodeList[x];
    switch (node.nodeName)
    {
      case "validity":
        validity = node.childNodes[0].nodeValue;
        break;
      case "aging":
        age = parseFloat(node.childNodes[0].nodeValue);
        break;
      case "ccs811_eco2":
        val_eco2 = node.childNodes[0].nodeValue;
        break;
      case "ccs811_tvoc":
        val_tvoc = node.childNodes[0].nodeValue;
        break;
    }
  }
  if (validity = "DATA_VALID")
  {
    if (age < 10)
    {
      text += "<tr><td>" + val_eco2 + "</td><td>" + val_tvoc;
    }
    else
    {
      text += "<tr><td class=\"stale\">" + val_eco2 + "</td><td class=\"stale\">" + val_tvoc;
    }
  }
  else
  {
    text += "<tr><td class=\"error\">" + validity + "</td><td class=\"error\">" + validity;
  }
  text += "</td></tr></table>";
  return text;
}

function add_TSL2561(nodeList)
{
  var x;
  var node;
  var text = "<table><tr><th>Data 0</th><th>Data 1</th><th>Lux</th></tr>";
  var val_data0;
  var val_data1;
  var val_lux;
  var validity;
  var age;

  //
  // Yes, this is a "for-case" type structure.  It's done this way because it's
  // possible that the order of the nodes may change.
  //
  for (x = 0; x < nodeList.length; x++)
  {
    node = nodeList[x];
    switch (node.nodeName)
    {
      case "validity":
        validity = node.childNodes[0].nodeValue;
        break;
      case "aging":
        age = parseFloat(node.childNodes[0].nodeValue);
        break;
      case "tsl2561_data0":
        val_data0 = node.childNodes[0].nodeValue;
        break;
      case "tsl2561_data1":
        val_data1 = node.childNodes[0].nodeValue;
        break;
      case "tsl2561_lux":
        val_lux = node.childNodes[0].nodeValue;
        break;
    }
  }
  if (validity = "DATA_VALID")
  {
    if (age < 10)
    {
      text += "<tr><td>" + val_data0 + "</td><td>" + val_data1 + "</td><td>" + val_lux;
    }
    else
    {
      text += "<tr><td class=\"stale\">" + val_data0 + "</td><td class=\"stale\">" +
              val_data1 + "</td><td class=\"stale\">" + val_lux;
    }
  }
  else
  {
    text += "<tr><td class=\"error\">" + validity + "</td><td class=\"error\">" +
            validity + "</td><td class=\"error\">" + validity;
  }
    text += "</td></tr></table>";
  return text;
}
