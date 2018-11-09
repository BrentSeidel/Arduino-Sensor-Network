with Ada.Sequential_IO;
with Ada.Characters.Latin_1;
with Ada.Calendar;
with Ada.Containers;
use type Ada.Containers.Count_Type;
with Ada.Containers.Vectors;
with BBS.embed;
use type BBS.embed.int8;
use type BBS.embed.uint32;
with BBS.web_common;
with common;
use type common.data_record;
use type common.message_types;

package rs485 is


   --
   --  The data is stored in a vector of data_record which stores data from all
   --  the addresses on a single device.  These vectors are stored in another
   --  vector with one entry per device.
   --
   package device_vect is new Ada.Containers.Vectors
     (Index_Type   => Natural,
      Element_Type => common.data_record);

   --
   --  Record for containing device information
   --
   type device_record is
      record
         info_age : Ada.Calendar.Time;  -- Time that last info record was received.
         last_age : Ada.Calendar.Time;  -- Time that last data was received
         num_addr : BBS.embed.uint32;   -- Number of addresses supported by device
         name     : String(1..32);      -- Device name
         messages : device_vect.Vector; -- Vector of message records
      end record;

   package data_vect is new Ada.Containers.Vectors
     (Index_Type   => Natural,
      Element_Type => device_record);
   --
   --  Make the data_store_type a protected type since it is being updated by the
   --  RS-485 state machine task and read by the various web request handler
   --  tasks.  This should help to prevent any problems trying to read the data
   --  while it is being updated.
   --
   protected type data_store_type is
      --
      --  Update the globally accessible data store.  The sizes of the vectors
      --  should be fairly stable so the appends only occur early in execution.
      --  Once the full set of data has been received, no more should be required.
      --  Note that once a device has been seen, it will have an entry even if it
      --  nevers shows up again.  This should only be called in the RS485 package
      --  from the state_machine task.
      --
      procedure update_data_store(data_in : common.data_record; dev : BBS.embed.uint32;
                                  addr : BBS.embed.uint32);
      --
      --  Extract an element from the data store.
      --
      function get_element(dev : Natural; addr : Natural)
                           return common.data_record;
      --
      --  Determine the length of the data store itself.
      --
      function get_length return Ada.Containers.Count_Type;
      --
      --  Since the data store is a vector of vectors, determine the length of
      --  one of the vectors in the data store.
      --
      function get_length(dev : Natural) return Ada.Containers.Count_Type;
      --
      -- Return the device record
      --
      function get_device(dev : Natural) return device_record;
   private
      data : data_vect.Vector := data_vect.Empty_Vector;
   end data_store_type;

   data_store : data_store_type;
   --
   --  Define the task to run the RS-485 state machine.  This reads from the
   --  bus, analyzes the protocol, parses the data, and stores it in data_store.
   --
   task state_machine is
      entry start;
      entry stop;
   end state_machine;

   activity_counter : BBS.embed.uint32
     with Volatile;

   --
   --  Function to convert numeric code to validity
   --
   function code_to_validity(c : BBS.embed.uint32) return common.msg_validity
     with Global => Null;
   --
   --  Interface to output to send commands to the RS-485 controller.  It is in
   --  a task to prevent multiple other tasks from simultaneously trying to send
   --  a command and having garbled output.
   --
   task command_task is
      entry start;
      entry stop;
      entry send_cmd(cmd : String);
   end command_task;

   --
   --  Flags to control display of debugging messages
   --
   debug_msg  : BBS.web_common.protected_flag; -- Identify messages received
   debug_char : BBS.web_common.protected_flag; -- Display characters received
   --
   --  Flags to indicate failure of the RS-485 tasks.
   --
   failed_state : BBS.web_common.protected_flag;
   failed_cmd   : BBS.web_common.protected_flag;

private
   --
   --  The tty port that is reading from the RS-485 bus.
   --
   rs485_port : constant String := "/dev/ttyO4";
   --
   -- Define the two control characters that are used.
   --
   CR : constant Character := Ada.Characters.Latin_1.CR;
   LF : constant Character := Ada.Characters.Latin_1.LF;
   --
   --  Define the states for the state machine
   --
   type states is (STATE_START, STATE_GET_MSG_DEV, STATE_GET_MSG_ADDR,
                   STATE_GET_MSG_TYPE, STATE_START_BUFFER, STATE_BUFFER_ENTRY,
                   STATE_BUFFER_END, STATE_WAIT_MSG_CR, STATE_GET_CMD_DEV,
                   STATE_GET_CMD_ADDR, STATE_GET_CMD_CODE, STATE_GET_CMD_ARG,
                   STATE_WAIT_CMD_CR);
   --
   --  State machine result values
   --
   type statuses is (STATE_RS485_PROCESSING, STATE_RS485_GOT_CMD,
                     STATE_RS485_GOT_MSG);
   --
   --  Data buffer returned from state machine.
   --
   type data_buffer_type is array (0 .. 31) of BBS.embed.uint32;

   --
   --  State machine support functions
   --
   procedure wait_for_cr(c : Character; s : in out states; new_state : states);
   function get_hex_char(c : Character) return BBS.embed.int8;
   --
   --  Functions to parse out the data buffer
   --
   function parse_msg_info(d : data_buffer_type) return common.data_record;
   function parse_msg_discrete(d : data_buffer_type) return common.data_record;
   function parse_msg_analog(d : data_buffer_type) return common.data_record;
   function parse_msg_BME280(d : data_buffer_type) return common.data_record;
   function parse_msg_CCS811(d : data_buffer_type) return common.data_record;
   function parse_msg_TSL2561(d : data_buffer_type) return common.data_record;
   function parse_msg_PCA9685(d : data_buffer_type) return common.data_record;
   function parse_msg_unknown(d : data_buffer_type) return common.data_record;

end;
