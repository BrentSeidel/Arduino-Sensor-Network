"use strict";
//
// Global variables
//
var unitMetric = 1;  // Display in metric or imperial units
var deviceId = -1;   // Which device is currently being displayed
var dispTimer;       // Timer used for reloading the display
var numDevices = 0;  // Number of devices on RS-485 network
var therm_timer;     // Timer for display refresh
//
// When the HTML page is loaded, this is called to start a timer to periodically
// refresh the display.
//
function startReload()
{
  loadNames();
  therm_timer = window.setInterval(reloadDisplay, 5000);
}
//
//  This is called everytime the timer fires.  It updates the data being displayed.
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
//  Request the name for a specific device.
//
function sendDeviceRequest(dev)
{
  var xhttp = new XMLHttpRequest();

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
//  Display the returned name (or an error) for a specific device.
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
//  Display the data for a specific device
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
  //
  // Discretes
  //
  temp = xmlDoc.getElementsByTagName("discrete");
  if (temp.length > 0)
  {
    add_Discretes(temp[0].childNodes);
    document.getElementById("Discretes").style.display = "block";
  }
  else
  {
    document.getElementById("Discretes").style.display = "none";
  }
  //
  // Analogs
  //
  temp = xmlDoc.getElementsByTagName("analogs");
  if (temp.length > 0)
  {
    add_Analogs(temp[0].childNodes);
    document.getElementById("Analogs").style.display = "block";
  }
  else
  {
    document.getElementById("Analogs").style.display = "none";
  }
  //
  // BME280
  //
  temp = xmlDoc.getElementsByTagName("bme280");
  if (temp.length > 0)
  {
    add_BME280(temp[0].childNodes);
    document.getElementById("BME280").style.display = "block";
  }
  else
  {
    document.getElementById("BME280").style.display = "none";
  }
  //
  // CCS811
  //
  temp = xmlDoc.getElementsByTagName("ccs811");
  if (temp.length > 0)
  {
    add_CCS811(temp[0].childNodes);
    document.getElementById("CCS811").style.display = "block";
  }
  else
  {
    document.getElementById("CCS811").style.display = "none";
  }
  //
  // TSL2561
  //
  temp = xmlDoc.getElementsByTagName("tsl2561");
  if (temp.length > 0)
  {
    add_TSL2561(temp[0].childNodes);
    document.getElementById("TSL2561").style.display = "block";
  }
  else
  {
    document.getElementById("TSL2561").style.display = "none";
  }
}
//
// Helper functions to display specific kinds of data.
//
function add_Discretes(nodeList)
{
  var x;
  var i;
  var j;
  var type;
  var value;
  var text;
  var node;

  //
  // Yes, this is a "for-case" type structure.  It's done this way because it's
  // possible that the order of the nodes may change.
  //
  for (x = 0; x < nodeList.length; x++)
  {
    node = nodeList[x];
    switch (node.nodeName)
    {
      case "disc_type":
        type = node.childNodes[0].nodeValue;
        break;
      case "disc_value":
        value = parseFloat(node.childNodes[0].nodeValue);
        break;
    }
  }
  text = "<p>Discretes value is " + value.toString(16) + ".</p><table>";
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
  document.getElementById("Discretes").innerHTML = text;
}

function add_Analogs(nodeList)
{
  var i;
  var type;
  var count;
  var text;
  var node;
  var data = [];

  //
  // Yes, this is a "for-case" type structure.  It's done this way because it's
  // possible that the order of the nodes may change.
  //
  for (i = 0; i < nodeList.length; i++)
  {
    node = nodeList[i];
    switch (node.nodeName)
    {
      case "analog_type":
        type = node.childNodes[0].nodeValue;
        break;
      case "analog_count":
        count = parseFloat(node.childNodes[0].nodeValue);
        break;
      case "value":
        data.push(node.childNodes[0].nodeValue);
        break;
    }
  }
  if (count == 1)
  {
    text = "<p>" + count + " Analog value found</p>";
  }
  else
  {
    text = "<p>" + count + " Analog values found</p>";
  }
  text += "<table><tr><td>Index</td><td>Value</td></tr>";
  for (i = 0; i < data.length; i++)
  {
    text += "<tr><td>" + i + "</td><td>" + data[i] + "</td></tr>";
  }
  text += "</table>";
  document.getElementById("Analogs").innerHTML = text;
}

function add_BME280(nodeList)
{
  var x;
  var node;
  var temp_c;
  var temp_f;
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
      document.getElementById("BME280-tempG").innerHTML =
        "<img src=\"/Thermometer?min=0&max=100&value=" + Math.round(temp_c) + "\"/>";
      document.getElementById("BME280-pressG").innerHTML =
        "<img src=\"/Thermometer?min=90000&max=100000&value=" + Math.round(pressure) + "\"/>";
      document.getElementById("BME280-tempT").innerHTML = temp_c.toFixed(1) + "&deg;C";
      document.getElementById("BME280-pressT").innerHTML = Math.round(pressure) + "Pa";
    }
    else
    {
      temp_f = (temp_c*9/5) + 32;
      pressure = pressure/3386.39;
      document.getElementById("BME280-tempG").innerHTML =
        "<img src=\"/Thermometer?min=50&max=150&value=" + Math.round(temp_f) + "\"/>";
      document.getElementById("BME280-pressG").innerHTML =
        "<img src=\"/Thermometer?min=2500&max=3500&value=" + Math.round(pressure*100.0) +
        "\"/>";
      document.getElementById("BME280-tempT").innerHTML = temp_f.toFixed(1) + "&deg;F";
      document.getElementById("BME280-pressT").innerHTML = pressure.toFixed(2) + "inHg";
    }
    document.getElementById("BME280-humG").innerHTML =
      "<img src=\"/Dial?min=0&max=100&value=" + Math.round(humidity) + "\"/>";
    document.getElementById("BME280-humT").innerHTML = humidity.toFixed(1) + "%";
  }
  else
  {
    td_class += " error";
    document.getElementById("BME280-tempG").innerHTML =
      "<img src=\"/Thermometer?min=hello\"/>";
    document.getElementById("BME280-pressG").innerHTML =
      "<img src=\"/Thermometer?min=hello\"/>";
    document.getElementById("BME280-humG").innerHTML = "<img src=\"/Dial?min=hello\"/>";
    document.getElementById("BME280-tempT").innerHTML = validity + "&deg;F";
    document.getElementById("BME280-pressT").innerHTML = validity + "inHg";
    document.getElementById("BME280-humT").innerHTML = validity + "%";
  }
  document.getElementById("BME280-tempG").className = td_class;
  document.getElementById("BME280-pressG").className = td_class;
  document.getElementById("BME280-humG").className = td_class;
  document.getElementById("BME280-tempT").className = td_class;
  document.getElementById("BME280-pressT").className = td_class;
  document.getElementById("BME280-humT").className = td_class;
}

function add_CCS811(nodeList)
{
  var x;
  var node;
  var val_eco2;
  var val_tvoc;
  var validity;
  var age;
  var td_class;

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
    if (age >= 10)
    {
      td_class = "stale";
    }
    else
    {
      td_class = "ok";
    }
    document.getElementById("CCS811-CO2").innerHTML = val_eco2;
    document.getElementById("CCS811-TVOC").innerHTML = val_tvoc;
  }
  else
  {
    td_class = "error";
    document.getElementById("CCS811-CO2").innerHTML = validity;
    document.getElementById("CCS811-TVOC").innerHTML = validity;
  }
  document.getElementById("CCS811-CO2").className = td_class;
  document.getElementById("CCS811-TVOC").className = td_class;
  return;
}

function add_TSL2561(nodeList)
{
  var x;
  var node;
  var val_data0;
  var val_data1;
  var val_lux;
  var validity;
  var age;
  var td_class;

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
    if (age >= 10)
    {
      td_class = "stale";
    }
    else
    {
      td_class = "ok";
    }
    document.getElementById("TSL2561-Data0").innerHTML = val_data0;
    document.getElementById("TSL2561-Data1").innerHTML = val_data1;
    document.getElementById("TSL2561-Lux").innerHTML = val_lux;
  }
  else
  {
    td_class = "error";
    document.getElementById("TSL2561-Data0").innerHTML = validity;
    document.getElementById("TSL2561-Data1").innerHTML = validity;
    document.getElementById("TSL2561-Lux").innerHTML = validity;
  }
  document.getElementById("TSL2561-Data0").className = td_class;
  document.getElementById("TSL2561-Data1").className = td_class;
  document.getElementById("TSL2561-Lux").className = td_class;
  return;
}
