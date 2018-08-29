package body rs485 is
   --
   -- Function to convert numeric code to validity
   --
   function code_to_validity(c : Integer) return msg_validity is
      t : msg_validity;
   begin
      case c is
         when 0 =>
            t := DATA_VALID;
         when 1 =>
            t := DATA_STALE;
         when 2 =>
            t := DATA_INIT;
         when 3 =>
            t := DATA_SENSOR;
         when 4 =>
            t := DATA_NO_COMPUTED;
         when others =>
            t := DATA_INVALID;
      end case;
      return t;
   end;

   --
   -- Task to read from the RS-485 interface
   --
   task body state_machine is
      buff : Character;
      file : Char_IO.File_Type;
      rs485_state : states := STATE_START;
      status : statuses;
      data_type : message_types;
      data_type_int : BBS.embed.uint32;
      device : BBS.embed.uint32;
      address : BBS.embed.uint32;
      data_buffer : data_buffer_type;
      buff_ptr : Integer := 0;
      t : BBS.embed.int8;
      temp_rec : data_record;
   begin
      accept start do
         null;
      end start;
      Ada.Text_IO.Put_Line("Starting RS-485 state machine.");
      Char_IO.Open(File     => file,
                   Mode     => Char_IO.In_File,
                   Name     => input_port);
      while not Char_IO.End_Of_File(file) loop
         Char_IO.Read(file, buff);
         activity_counter := activity_counter + 1;
--         Ada.Text_IO.Put(buff);
         case rs485_state is
            when STATE_START =>
               status := STATE_RS485_PROCESSING;
               if (buff = '@') then
                  rs485_state := STATE_GET_CMD_DEV;
               elsif (buff = '#') then
                  rs485_state := STATE_GET_MSG_DEV;
               end if;
               device := 0;
            when STATE_GET_MSG_DEV =>
               if (buff = '/') then
                  rs485_state := STATE_GET_MSG_ADDR;
                  address := 0;
               else
                  t := get_hex_char(buff);
                  if (t = -1) then
                     rs485_state := STATE_START;
                  else
                     device := device*16 + BBS.embed.uint32(t);
                  end if;
               end if;
            when STATE_GET_MSG_ADDR =>
               if (buff = '/') then
                  rs485_state := STATE_GET_MSG_TYPE;
                  data_type := MSG_TYPE_UNKNOWN;
                  data_type_int := 0;
               elsif (buff = '&') then
                  rs485_state := STATE_START_BUFFER;
                  for buff_ptr in data_buffer'range loop
                     data_buffer(buff_ptr) := 0;
                  end loop;
                  buff_ptr := 0;
               elsif (buff = '%') then
                  rs485_state := STATE_START;
               else
                  t := get_hex_char(buff);
                  if (t = -1) then
                     rs485_state := STATE_START;
                  else
                     address := address*16 + BBS.embed.uint32(t);
                  end if;
               end if;
            when STATE_GET_MSG_TYPE =>
               if (buff = '&') then
                  rs485_state := STATE_START_BUFFER;
                  for buff_ptr in data_buffer'range loop
                     data_buffer(buff_ptr) := 0;
                  end loop;
                  buff_ptr := 0;
               elsif (buff = '%') then
                  rs485_state := STATE_START;
               else
                  t := get_hex_char(buff);
                  if (t = -1) then
                     rs485_state := STATE_START;
                  else
                     data_type_int := data_type_int*16 + BBS.embed.uint32(t);
                  end if;
               end if;
            when STATE_START_BUFFER | STATE_BUFFER_ENTRY =>
               if (buff = '&') then
                  buff_ptr := buff_ptr + 1;
               elsif (buff = '%') then
                  rs485_state := STATE_BUFFER_END;
               else
                  t := get_hex_char(buff);
                  if (t = -1) then
                     rs485_state := STATE_START;
                  else
                     data_buffer(buff_ptr) := data_buffer(buff_ptr)*16 +
                       BBS.embed.uint32(t);
                  end if;
               end if;
            when STATE_BUFFER_END =>
               rs485_state := STATE_WAIT_MSG_LF;
            when STATE_WAIT_MSG_LF =>
               wait_for_lf(buff, rs485_state, STATE_START);
               if (rs485_state = STATE_START) then
                  status := STATE_RS485_GOT_MSG;
               end if;
            when STATE_GET_CMD_DEV =>
               if (buff = '/') then
                  rs485_state := STATE_GET_CMD_ADDR;
                  address := 0;
               else
                  t := get_hex_char(buff);
                  if (t = -1) then
                     rs485_state := STATE_START;
                  else
                     device := device*16 + BBS.embed.uint32(t);
                  end if;
               end if;
            when STATE_GET_CMD_ADDR =>
               if (buff = '/') then
                  rs485_state := STATE_START;
               elsif (buff = '&') then
                  rs485_state := STATE_START;
               elsif (buff = '%') then
                  rs485_state := STATE_WAIT_CMD_LF;
               else
                  t := get_hex_char(buff);
                  if (t = -1) then
                     rs485_state := STATE_START;
                  else
                     address := address*16 + BBS.embed.uint32(t);
                  end if;
               end if;
            when STATE_WAIT_CMD_LF =>
               wait_for_lf(buff, rs485_state, STATE_START);
               if (rs485_state = STATE_START) then
                  status := STATE_RS485_GOT_CMD;
               end if;
            when others =>
               rs485_state := STATE_START;
         end case; -- End of main state machine
         if (status = STATE_RS485_GOT_CMD) then
            Ada.Text_IO.Put_Line("Got cmd for device " & Integer'Image(Integer(device)) &
                                   ", address " & Integer'Image(Integer(address)));
         elsif (status = STATE_RS485_GOT_MSG) then
            data_type := message_types'Val(Integer(data_type_int));
            Ada.Text_IO.Put_Line("Got message from device " & Integer'Image(Integer(device)) &
                                   ", address " & Integer'Image(Integer(address)));
            case data_type is
               when MSG_TYPE_INFO =>
                  temp_rec := parse_msg_info(data_buffer);
               when MSG_TYPE_DISCRETE =>
                  temp_rec := parse_msg_discrete(data_buffer);
               when MSG_TYPE_BME280 =>
                  temp_rec := parse_msg_BME280(data_buffer);
               when MSG_TYPE_CCS811 =>
                  temp_rec := parse_msg_CCS811(data_buffer);
               when MSG_TYPE_TSL2561 =>
                  temp_rec := parse_msg_TSL2561(data_buffer);
               when others =>
                  temp_rec := parse_msg_unknown(data_buffer);
            end case;
            update_data_store(temp_rec, device, address);
         end if;
      end loop;
   end state_machine;

   procedure wait_for_lf(c : Character; s : in out states; new_state : states) is
   begin
      if (c = LF) then
         s := new_state;
      end if;
   end;

   function get_hex_char(c : Character) return BBS.embed.int8 is
      n : BBS.embed.int8 := -1;
   begin
      case c is
         when '0' =>
            n := 0;
         when '1' =>
            n := 1;
         when '2' =>
            n := 2;
         when '3' =>
            n := 3;
         when '4' =>
            n := 4;
         when '5' =>
            n := 5;
         when '6' =>
            n := 6;
         when '7' =>
            n := 7;
         when '8' =>
            n := 8;
         when '9' =>
            n := 9;
         when 'a' | 'A' =>
            n := 10;
         when 'b' | 'B' =>
            n := 11;
         when 'c' | 'C' =>
            n := 12;
         when 'd' | 'D' =>
            n := 13;
         when 'e' | 'E' =>
            n := 14;
         when 'f' | 'F' =>
            n := 15;
         when others => -- Any other character is an error
            n := -1;
      end case;
      return n;
   end;

   function parse_msg_info(d : data_buffer_type) return data_record is
      t : String(1..32);
      temp : BBS.embed.uint32;
      b1 : Integer;
      b2 : Integer;
      b3 : Integer;
      b4 : Integer;
   begin
      for i in Integer Range 0 .. 7 loop
         temp := d(i + 1);
         b4 := Integer(temp and 16#ff#);
         temp := temp / 16#100#;
         b3 := Integer(temp and 16#ff#);
         temp := temp / 16#100#;
         b2 := Integer(temp and 16#ff#);
         temp := temp / 16#100#;
         b1 := Integer(temp and 16#ff#);
         temp := temp / 16#100#;
         t(i*4 + 1) := Character'Val(b1);
         t(i*4 + 2) := Character'Val(b2);
         t(i*4 + 3) := Character'Val(b3);
         t(i*4 + 4) := Character'Val(b4);
      end loop;
      return (validity => DATA_VALID, aging => Ada.Calendar.Clock,
              message => MSG_TYPE_INFO, num_addr => d(0), name => t);
   end;

   function parse_msg_BME280(d : data_buffer_type) return data_record is
   begin
      return (validity => code_to_validity(Integer(d(0))), aging => Ada.Calendar.Clock,
              message => MSG_TYPE_BME280,
              BME280_status => d(0),
              BME280_age => d(1),
              BME280_temp_c => Float(d(2)*5+128)/25600.0,
              BME280_pressure_pa => Float(d(3))/256.0,
              BME280_humidity => Float(d(4))/1024.0);
   end;

   function parse_msg_discrete(d : data_buffer_type) return data_record is
   begin
      return (validity => DATA_VALID, aging => Ada.Calendar.Clock,
              message => MSG_TYPE_DISCRETE,
              disc_type => d(0),
              disc_value => d(1));
   end;

   function parse_msg_CCS811(d : data_buffer_type) return data_record is
   begin
      return (validity => code_to_validity(Integer(d(0))), aging => Ada.Calendar.Clock,
              message => MSG_TYPE_CCS811,
              CCS811_status => d(0),
              CCS811_age => d(1),
              CCS811_eCO2 => d(2),
              CCS811_TVOC => d(3));
   end;

   function parse_msg_TSL2561(d : data_buffer_type) return data_record is
   begin
      return (validity => code_to_validity(Integer(d(0))), aging => Ada.Calendar.Clock,
              message => MSG_TYPE_TSL2561,
              TSL2561_status => d(0),
              TSL2561_age => d(1),
              TSL2561_data0 => d(2),
              TSL2561_data1 => d(3),
              TSL2561_lux => d(4));
   end;

   function parse_msg_unknown(d : data_buffer_type) return data_record is
   begin
      return (validity => DATA_VALID, aging => Ada.Calendar.Clock,
              message => MSG_TYPE_UNKNOWN);
   end;

   --
   -- Update the globally accessible data store.  The sizes of the vectors
   -- should be fairly stable so the appends only occur early in execution.
   -- Once the full set of data has been received, no more should be required.
   -- Note that once a device has been seen, it will have an entry even if it
   -- nevers shows up again.
   --
   procedure update_data_store(data : data_record; dev : BBS.embed.uint32;
                               addr : BBS.embed.uint32) is
      d : Natural := Natural(dev);
      a : Natural := Natural(addr);
      t : device_vect.Vector := device_vect.Empty_Vector;
   begin
      --
      -- Make sure that the device vector is long enough
      --
      while (data_store.Length <= Ada.Containers.Count_Type(d)) loop
         data_store.Append(device_vect.Empty_Vector);
      end loop;
      --
      -- Make sure that the data vector for the device is long enough
      --
      t := data_store.Element(d);
      while (t.Length <= Ada.Containers.Count_Type(a)) loop
         t.Append((Validity => DATA_VALID, aging => Ada.Calendar.Clock,
                               message => MSG_TYPE_UNKNOWN));
      end loop;
      t.Replace_Element(a, data);
      data_store.Replace_Element(d, t);
   end;

end;
