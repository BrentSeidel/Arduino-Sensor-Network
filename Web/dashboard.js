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
  var temp_c;
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
    text += "<table><tr><th>Temperature</th><th>Pressure</th><th>Humidity</th></tr>";
    temp_c = parseFloat(xmlDoc.getElementsByTagName("bme280_temp_c")[0].childNodes[0].nodeValue);
    pressure = xmlDoc.getElementsByTagName("bme280_pressure_pa")[0].childNodes[0].nodeValue;
    humidity = parseFloat(xmlDoc.getElementsByTagName("bme280_humidity")[0].childNodes[0].nodeValue);
    text += "<tr><td><img src=\"/Thermometer?min=0&max=100&value=" +
            Math.round(temp_c) + "\"></img></td>";
    text += "<td><img src=\"/Thermometer?min=90000&max=100000&value=" +
            Math.round(pressure) + "\"></img></td>";
    text += "<td><img src=\"/Dial?min=0&max=100&value=" + Math.round(humidity) +
           "\"></img></td></tr>";
    text += "<tr><td>" + temp_c.toFixed(1) + "&deg;C</td>";
    text += "<td>" + Math.round(pressure) + "Pa</td>";
    text += "<td>" + humidity.toFixed(1) + "%</td></tr>";
    text += "</table>";
  }
  //
  // CCS811
  //
  temp = xmlDoc.getElementsByTagName("ccs811");
  if (temp.length > 0)
  {
    text += "<table><tr><th>CO<sub>2</sub></th><th>TVOC</th></tr>";
    text += "<tr><td>" + xmlDoc.getElementsByTagName("ccs811_eco2")[0].childNodes[0].nodeValue;
    text += "</td><td>" + xmlDoc.getElementsByTagName("ccs811_tvoc")[0].childNodes[0].nodeValue;
    text += "</td></tr></table>";
  }
  //
  // TSL2561
  //
  temp = xmlDoc.getElementsByTagName("tsl2561");
  if (temp.length > 0)
  {
    text += "<table><tr><th>Data 0</th><th>Data 1</th><th>Lux</th></tr>";
    text += "<tr><td>" + xmlDoc.getElementsByTagName("tsl2561_data0")[0].childNodes[0].nodeValue;
    text += "</td><td>" + xmlDoc.getElementsByTagName("tsl2561_data1")[0].childNodes[0].nodeValue;
    text += "</td><td>" + xmlDoc.getElementsByTagName("tsl2561_lux")[0].childNodes[0].nodeValue;
    text += "</td></tr></table>";
  }
  document.getElementById("DevData").innerHTML = text;
}
