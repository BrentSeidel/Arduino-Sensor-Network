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

  if (error.length > 0)
  {
    text = error[0].childNodes[0].nodeValue;
  }
  else
  {
    text = "<h1>" + name[0].childNodes[0].nodeValue + "</h1>";
  }
  temp = xmlDoc.getElementsByTagName("bme280");
  if (temp.length > 0)
  {
    text += "<table><td><th>Temperature</th><th>Pressure</th><th>Humidity</th></tr>";
    temp_c = parseFloat(xmlDoc.getElementsByTagName("bme280_temp_c")[0].childNodes[0].nodeValue);
    pressure = xmlDoc.getElementsByTagName("bme280_pressure_pa")[0].childNodes[0].nodeValue;
    humidity = parseFloat(xmlDoc.getElementsByTagName("bme280_humidity")[0].childNodes[0].nodeValue);
    text += "<tr><td><img src=\"/Thermometer?min=0&max=100&value=" +
            Math.round(temp_c) + "\"></img></td>";
    text += "<td>Pressure goes here</td>";
    text += "<td><img src=\"/Dial?min=0&max=100&value=" + Math.round(humidity) +
           "\"></img></td></tr>";
    text += "<tr><td>" + temp_c.toFixed(1) + "&deg;C</td>";
    text += "<td>" + Math.round(pressure) + "Pa</td>";
    text += "<td>" + humidity.toFixed(1) + "%</td></tr>";
    text += "</table>";
  }
  document.getElementById("DevData").innerHTML = text;
}
//
// Global values for some timer related functions.
//
var therm_value;
var therm_timer;
//
// Initializes the value and starts a timer to count up.  Apparently this can't
// just be called "animate".  Perhaps some browsers define it and some don't.
//
function animateThings()
{

  alert("Animate called.");
  therm_value = 0;
  therm_timer = window.setInterval(update_therm_up, 250);
}
//
// Counts up and updates a couple images on each count.
//
function update_therm_up()
{
  var el1 = document.getElementById("temp1");
  var el2 = document.getElementById("temp2");
  var el3 = document.getElementById("dial1");
  el1.src = "/Thermometer?min=100&max=350&value=" + (350 - therm_value);
  el2.src = "/Thermometer?min=0&max=250&value=" + therm_value;
  el3.src = "/Dial?min=100&max=200&value=" + therm_value;
  therm_value++;
  if (therm_value > 250)
  {
    window.clearInterval(therm_timer);
  therm_timer = window.setInterval(update_therm_down, 250);
  }
}
//
// Counts down and updates a couple images on each count.
//
function update_therm_down()
{
  var el1 = document.getElementById("temp1");
  var el2 = document.getElementById("temp2");
  var el3 = document.getElementById("dial1");
  el1.src = "/Thermometer?min=100&max=350&value=" + (350 - therm_value);
  el2.src = "/Thermometer?min=0&max=250&value=" + therm_value;
  el3.src = "/Dial?min=100&max=200&value=" + therm_value;
  therm_value--;
  if (therm_value < 0)
  {
    window.clearInterval(therm_timer);
  }
}
