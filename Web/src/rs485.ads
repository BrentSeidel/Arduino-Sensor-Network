with Ada.Text_IO;
with Ada.Sequential_IO;
with Ada.Characters.Latin_1;
with Ada.Strings.Fixed;
with Ada.Calendar;
with Ada.Containers;
use type Ada.Containers.Count_Type;
with Ada.Containers.Vectors;
with web_common;
with BBS.embed;
with BBS.units;
use type BBS.embed.int8;
use type BBS.embed.uint8;
use type BBS.embed.int32;
use type BBS.embed.uint32;

package rs485 is

   type message_types is (MSG_TYPE_UNKNOWN, MSG_TYPE_EMPTY, MSG_TYPE_NAK,
                          MSG_TYPE_INFO,  MSG_TYPE_BME280,  MSG_TYPE_DISCRETE,
                          MSG_TYPE_ANALOG, MSG_TYPE_VERSION, MSG_TYPE_CCS811,
                          MSG_TYPE_TSL2561);

   type msg_validity is (DATA_VALID, DATA_STALE, DATA_INIT, DATA_SENSOR,
                         DATA_NO_COMPUTED, DATA_INVALID);

   type data_record (message : message_types := MSG_TYPE_UNKNOWN) is
      record
         validity : msg_validity; -- Validity code
         aging : Ada.Calendar.Time; -- Time record was created.
         case message is
            when MSG_TYPE_INFO =>
               num_addr : BBS.embed.uint32;
               name : String(1..32);
            when MSG_TYPE_BME280 =>
               BME280_status : BBS.embed.uint32;
               BME280_age : BBS.embed.uint32;
               BME280_temp_c : Float;
               BME280_pressure_pa : Float;
               BME280_humidity : Float;
            when MSG_TYPE_DISCRETE =>
               disc_type : BBS.embed.uint32;
               disc_value : BBS.embed.uint32;
            when MSG_TYPE_ANALOG =>
               null;
            when MSG_TYPE_VERSION =>
               null;
            when MSG_TYPE_CCS811 =>
               CCS811_status : BBS.embed.uint32;
               CCS811_age : BBS.embed.uint32;
               CCS811_eCO2 : BBS.embed.uint32;
               CCS811_TVOC : BBS.embed.uint32;
            when MSG_TYPE_TSL2561 =>
               TSL2561_status : BBS.embed.uint32;
               TSL2561_age : BBS.embed.uint32;
               TSL2561_data0 :  BBS.embed.uint32;
               TSL2561_data1 :  BBS.embed.uint32;
               TSL2561_lux :  BBS.embed.uint32;
            when others =>
               null;
         end case;
      end record;

   --
   -- The data is stored in a vector of data_record which stores data from all
   -- the addresses on a single device.  These vectors are stored in another
   -- vector with one entry per device.
   --
   package device_vect is new Ada.Containers.Vectors
     (Index_Type   => Natural,
      Element_Type => data_record);

   package data_vect is new Ada.Containers.Vectors
     (Index_Type   => Natural,
      Element_Type => device_vect.Vector,
     "=" => device_vect."=");
   --
   -- Data storage for data from the bus.  It should be only written to by the
   -- state_machine task.  It may be read elsewhere.
   --
   data_store : data_vect.Vector := data_vect.Empty_Vector;

   --
   -- Define the task to run the RS-485 state machine.  This reads from the
   -- bus, analyzes the protocol, parses the data, and stores it in data_store.
   --
   task state_machine is
      entry start;
   end state_machine;

   activity_counter : BBS.embed.uint32;

   --
   -- Function to convert numeric code to validity
   --
   function code_to_validity(c : Integer) return msg_validity;

private
   --
   -- The tty port that is reading from the RS-485 bus.
   --
   input_port : constant String := "/dev/ttyO4";
   --
   -- Instantiate the Generic Sequential I/O package to read characters
   --
   package Char_IO is new Ada.Sequential_IO(Character);
   --
   -- Define the two control characters that are used.
   --
   CR : constant Character := Ada.Characters.Latin_1.CR;
   LF : constant Character := Ada.Characters.Latin_1.LF;

   type states is (STATE_START, STATE_GET_MSG_DEV, STATE_GET_MSG_ADDR,
                   STATE_GET_MSG_TYPE, STATE_START_BUFFER, STATE_BUFFER_ENTRY,
                   STATE_BUFFER_END, STATE_WAIT_MSG_LF, STATE_GET_CMD_DEV,
                   STATE_GET_CMD_ADDR, STATE_WAIT_CMD_LF);
   type statuses is (STATE_RS485_PROCESSING, STATE_RS485_GOT_CMD,
                     STATE_RS485_GOT_MSG);

   type data_buffer_type is array (0 .. 31) of BBS.embed.uint32;

   --
   -- State machine support functions
   --
   procedure wait_for_lf(c : Character; s : in out states; new_state : states);
   function get_hex_char(c : Character) return BBS.embed.int8;
   --
   -- Functions to parse out the data buffer
   --
   function parse_msg_info(d : data_buffer_type) return data_record;
   function parse_msg_discrete(d : data_buffer_type) return data_record;
   function parse_msg_BME280(d : data_buffer_type) return data_record;
   function parse_msg_CCS811(d : data_buffer_type) return data_record;
   function parse_msg_TSL2561(d : data_buffer_type) return data_record;
   function parse_msg_unknown(d : data_buffer_type) return data_record;
   --
   -- Update the globally accessible data store
   --
   procedure update_data_store(data : data_record; dev : BBS.embed.uint32;
                               addr : BBS.embed.uint32);


end;
