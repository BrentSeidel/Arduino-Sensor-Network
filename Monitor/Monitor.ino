// include the library code:
#include <LiquidCrystal.h>
#include <RS-485.h>

/*
Monitors the serial port and displays characters on the 20x4
LCD.

  The circuit:
 * LCD RS pin to digital pin 7
 * LCD Enable pin to digital pin 8
 * LCD D4 pin to digital pin 9
 * LCD D5 pin to digital pin 10
 * LCD D6 pin to digital pin 11
 * LCD D7 pin to digital pin 12
 * LCD R/W pin to ground
 * LCD VSS pin to ground
 * LCD VCC pin to 5V
 * 10K resistor:
 * ends to +5V and ground
 * wiper to LCD VO pin (pin 3)

  Note that there is also a RS-485 transceiver wired to be receive only
  and connected to the RX pin.
*/

//
// initialize the library by associating any needed LCD interface pin
// with the arduino pin number it is connected to
//
const int rs = 7, en = 8, d4 = 9, d5 = 10, d6 = 11, d7 = 12;
LiquidCrystal lcd(rs, en, d4, d5, d6, d7);
//
// Delay in microseconds after clearing LCD
//
#define LCD_DELAY 1000

//
// Data for the lCD
//
char display[20][4];
int row = 0;
int column = 0;
//
// Change this to whichever port is hooked up to RS-485.  For the Uno, use the only
// serial port.  For the Mega and Due, use Serial3
//
#if defined (__AVR_ATmega328P__)
HardwareSerial *rs485 = &Serial;
#else
HardwareSerial *rs485 = &Serial3;
#endif
//#define NUM_NODES 8
#define NODE_NEVER 32767
const bool DEBUG = true;
//
// Data from controller
//
bool nodes_change = true;
unsigned int nodes = 0;
unsigned int options = 0;
//
// Data from Responder 1
//
bool light_change = true;
unsigned int light_value = 0;
unsigned int min_light = 0;
unsigned int max_light = 0;
uint32_t switch_reg = 0;
//
// Data from responder 2
//
int BME280_status[NUM_NODES + 1];
int BME280_age[NUM_NODES + 1];
int32_t  t_fine[NUM_NODES + 1];    // Calibrated temperature
uint32_t cal_press[NUM_NODES + 1]; // Calibrated pressure (Pa/256)
uint32_t cal_hum[NUM_NODES + 1];   // Calibrated humidity (%/1024)

#define TIMEOUT 500

const int TX_RX = 5; // Pin 5 is used to control direction.
//
// Switches
//
#define NUM_PINS 8
int switches[NUM_PINS] = {46, 48, 47, 50, 49, 51, 52, 53};

void setup()
{
  int temp;

  for (temp = 0; temp < NUM_PINS; temp++)
  {
    pinMode(switches[temp], INPUT_PULLUP);
  }
  Serial.begin(BAUD_RATE);
  rs485->begin(BAUD_RATE);
  pinMode(TX_RX, OUTPUT);
  digitalWrite(TX_RX, LOW);
  lcd.begin(20, 4);
  lcd.setCursor(0, 0);
  lcd.write("Waiting for data.");
  Serial.println("Monitor initialization complete.");
}

void loop()
{
  int x;
  int switch_value = 0;
  unsigned int selected_node;
  int selected_page;
  bool metric;

  for (x = 0; x < NUM_PINS; x++)
  {
    if (digitalRead(switches[x]))
    {
      switch_value += (1 << x);
    }
  }
  selected_node = switch_value & 7;
  selected_page = (switch_value >> 3) & 7;
  metric = (switch_value & 0x80) ? true : false;

  switch (rs485_state_machine(rs485, &Serial, DEBUG))
  {
    case STATE_RS485_PROCESSING: // Do nothing for this
      break;
    case STATE_RS485_GOT_CMD:  // Listener only.  Do nothing for commands
      break;
    case STATE_RS485_GOT_MSG:  // Looking for messages
      Serial.print("Got message from device ");
      Serial.print(device, DEC);
      Serial.print(", address ");
      Serial.println(address, DEC);
      switch (device)
      {
        case 0: // Controller device
          if (address == 1)
          {
            nodes_change = true;
            nodes = data_buffer[0];
            options = data_buffer[1];
          }
          break;
        case 1: // Responder device
        case 2: // Responder device
        case 3: // Responder device
        case 4: // Responder device
        case 5: // Responder device
        case 6: // Responder device
        case 7: // Responder device
          if (address == 1)
          {
            BME280_status[device] = data_buffer[0];
            BME280_age[device] = data_buffer[1];
            t_fine[device] = data_buffer[2];    // Calibrated temperature
            cal_press[device] = data_buffer[3]; // Calibrated pressure (Pa/256)
            cal_hum[device] = data_buffer[4];   // Calibrated humidity (%/1024)
          }
          break;
        case NODE_NEVER: // Should never happen
          if (address == 1)
          {
            light_change = true;
            light_value = data_buffer[0];
            min_light = data_buffer[1];
            max_light = data_buffer[2];
            switch_reg = data_buffer[3];
          }
          break;
        default:
          break;
      }
      switch (options)
      {
        case 0:
          if (nodes_change)
          {
            clear_display();
            lcd.write("Active Nodes ");
            lcd.print(nodes, HEX);
            lcd.setCursor(0, 1);
            for (x = 7; x >= 0; x--)
            {
              if (nodes & (1 << x))
              {
                lcd.write("X ");
              }
              else
              {
                lcd.write("O ");
              }
            }
            nodes_change = false;
          }
          break;
        case 1:
          if ((device == selected_node) || ((ages[device] + TIMEOUT) < millis()))
          {
            display_environ(device, metric);
          }
          break;
        case 2:
          if ((light_change) || ((ages[2] + TIMEOUT) < millis()))
          {
            clear_display();
            lcd.write("Light Level/");
            lcd.setCursor(0, 1);
            lcd.write("Value:         ");
            lcd.setCursor(0, 2);
            lcd.write("Minimum:       ");
            lcd.setCursor(0, 3);
            lcd.write("Maximum:       ");
            if (light_change)
            {
              lcd.setCursor(12, 0);
              lcd.print(switch_reg, HEX);
              lcd.setCursor(7, 1);
              lcd.print(light_value, DEC);
              lcd.setCursor(10, 2);
              lcd.print(min_light, DEC);
              lcd.setCursor(10, 3);
              lcd.print(max_light, DEC);
              light_change = false;
            }
            else
            {
              lcd.setCursor(7, 1);
              lcd.write("---");
              lcd.setCursor(10, 2);
              lcd.write("---");
              lcd.setCursor(10, 3);
              lcd.write("---");
              light_change = false;
            }
          }
          break;
        case 3: // Handled outside of state machine.
          clear_display();
          lcd.write("Option 3 is not");
          lcd.setCursor(0, 1);
          lcd.write("implemented");
          break;
        default:
          lcd.clear();
          delayMicroseconds(LCD_DELAY);
          lcd.write("Unrecognized option");
          lcd.setCursor(0, 1);
          lcd.print(options, DEC);
      }
      break;
  }
}

//
// Clears the display and buffer
//
void clear_display()
{
  int x;
  int y;

  lcd.clear();
//  delayMicroseconds(LCD_DELAY);
  row = 0;
  column = 0;
  for (x = 0; x < 20; x++)
  {
    for (y = 0; y < 4; y++)
    {
      display[x][y] = ' ';
    }
  }
}
//
// Scrolls the LCD display
//
void scroll_data()
{
  int x;
  int y;

  for (x = 0; x < 20; x++)
  {
    display[x][0] = display[x][1];
    display[x][1] = display[x][2];
    display[x][2] = display[x][3];
    display[x][3] = ' ';
  }
  lcd.clear();
  for (y = 0; y < 4; y++)
  {
    lcd.setCursor(0, y);
    for (x = 0; x < 20; x++)
    {
      lcd.write(display[x][y]);
    }
  }
  row = 3;
  column = 0;
  lcd.setCursor(column, row);
}
//
// Communication monitor mode
//
void monitor_mode(int data)
{
  //
  // Characters with a value less that 32 are control characters, not printing
  // characters.  They are all ignored, with the exception of '\n' which causes
  // a newline.
  //
  if (data > 0)
  {
    if (data == '\n')
    {
      lcd.setCursor(0, row++);
      if (row > 3)
      {
        scroll_data();
      }
    }
    else
    {
      if (data > 31)
      {
        lcd.write(data);
        display[column++][row] = data;
        if (column > 19)
        {
          column = 0;
          row++;
          if (row > 3)
          {
            scroll_data();
          }
          else
          {
            lcd.setCursor(column, row);
          }
        }
      }
    }
  }
}

void display_environ(int node, bool metric)
{
  clear_display();
  lcd.write("Temperature: ");
  lcd.setCursor(0, 1);
  lcd.write("Pressure:    ");
  lcd.setCursor(0, 2);
  lcd.write("Humidity:    ");
  lcd.setCursor(0, 3);
  lcd.write("Unit:        ");
  if ((ages[node] + TIMEOUT) >= millis())
  {
    if (metric)
    {
      String temperature = String(((t_fine[node]*5 + 128) >> 8)/100.0, 2);
      String pressure = String(cal_press[node]/25600.0, 2);
      String humidity = String(cal_hum[node]/1024.0, 2);

      lcd.setCursor(13, 0);
      lcd.print(temperature);
      lcd.write("C");
      lcd.setCursor(10, 1);
      lcd.print(pressure);
      lcd.write("hPa");
      lcd.setCursor(13, 2);
      lcd.print(humidity);
      lcd.write("%");
    }
    else
    {
      String temperature = String((((t_fine[node]*5 + 128) >> 8)/100.0)*9.0/5.0 + 32.0, 2);
      String pressure = String((cal_press[node]/256.0)/3386.39, 2);
      String humidity = String(cal_hum[node]/1024.0, 2);

      lcd.setCursor(13, 0);
      lcd.print(temperature);
      lcd.write("F");
      lcd.setCursor(10, 1);
      lcd.print(pressure);
      lcd.write("inHg");
      lcd.setCursor(13, 2);
      lcd.print(humidity);
      lcd.write("%");
    }
  }
  else
  {
    lcd.setCursor(14, 0);
    lcd.write("---");
    lcd.setCursor(14, 1);
    lcd.write("---");
    lcd.setCursor(14, 2);
    lcd.write("---");
  }
  lcd.setCursor(6, 3);
  lcd.print(node, DEC);
}


