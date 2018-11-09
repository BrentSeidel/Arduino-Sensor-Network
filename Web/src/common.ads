with Ada.Calendar;
with BBS.embed;
use type BBS.embed.int8;
use type BBS.embed.uint32;

--
--  Common items for the RS-495 web gateway
--
package common is
   --
   --  What kind of message has been received from the RS-485 bus.  This list
   --  must match the definitions for the communications protocol.  Note that
   --  not all of the defined messages may be implemented.
   --
   type message_types is (MSG_TYPE_UNKNOWN, MSG_TYPE_EMPTY, MSG_TYPE_NAK,
                          MSG_TYPE_INFO,  MSG_TYPE_BME280,  MSG_TYPE_DISCRETE,
                          MSG_TYPE_ANALOG, MSG_TYPE_VERSION, MSG_TYPE_CCS811,
                          MSG_TYPE_TSL2561, MSG_TYPE_PCA9685);

   --
   --  Validity code for messages from the RS-485 data bus.  Anything other than
   --  DATA_VALID indicates a failure somewhere and the data should not be trusted.
   --
   type msg_validity is (DATA_VALID, DATA_STALE, DATA_INIT, DATA_SENSOR,
                         DATA_NO_COMPUTED, DATA_INVALID);

   --
   --  Data type for arrays.  This is used because a defined data type is required
   --  as an array component.  You can't just use an anonymous array.
   --
   type an_data_type is array (1 .. 31) of BBS.embed.uint32;
   type arr_16bool is array (0 .. 15) of Boolean;
   type arr_16uint12 is array (0 .. 15) of BBS.embed.uint12;
   --
   --  Data record is a variant record that should support all types of data.
   --  The received message is translated into the appropriate variant of this
   --  record so that the data can be stored and passed around.
   --
   type data_record (message : message_types := MSG_TYPE_UNKNOWN) is
      record
         validity : msg_validity; -- Validity code
         aging : Ada.Calendar.Time; -- Time record was created.
         case message is
            when MSG_TYPE_INFO =>
               num_addr : BBS.embed.uint32;
               name : String(1..32);
            when MSG_TYPE_BME280 =>
               BME280_status : msg_validity;
               BME280_age : BBS.embed.uint32;
               BME280_temp_c : Float;
               BME280_pressure_pa : Float;
               BME280_humidity : Float;
            when MSG_TYPE_DISCRETE =>
               disc_type : BBS.embed.uint32;
               disc_value : BBS.embed.uint32;
            when MSG_TYPE_ANALOG =>
               an_type : BBS.embed.uint32;
               an_count : BBS.embed.uint32;
               an_data : an_data_type;
            when MSG_TYPE_VERSION =>
               null;
            when MSG_TYPE_CCS811 =>
               CCS811_status : msg_validity;
               CCS811_age : BBS.embed.uint32;
               CCS811_eCO2 : BBS.embed.uint32;
               CCS811_TVOC : BBS.embed.uint32;
            when MSG_TYPE_TSL2561 =>
               TSL2561_status : msg_validity;
               TSL2561_age : BBS.embed.uint32;
               TSL2561_data0 :  BBS.embed.uint32;
               TSL2561_data1 :  BBS.embed.uint32;
               TSL2561_lux :  BBS.embed.uint32;
            when MSG_TYPE_PCA9685 =>
               PCA9685_set : arr_16bool;
               PCA9685_on  : arr_16uint12;
               PCA9685_off : arr_16uint12;
            when others =>
               null;
         end case;
      end record;
end common;
