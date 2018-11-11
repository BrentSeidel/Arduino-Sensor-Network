with Ada.Calendar;
with Ada.Calendar.Formatting;
package body database is
   --
   --  Initialize the database interface by selecting which type of database to use.
   --
   procedure init(kind : database_type) is
      path : String := log_path & Ada.Calendar.Formatting.Image(Ada.Calendar.Clock) & "-";
   begin
      if (selected_db = None) then
         case kind is
         when CSV =>
            Ada.Text_IO.Create(File => Info_file,
                               Mode => Ada.Text_IO.Out_File,
                               Name => path & "Info.csv");
            Ada.Text_IO.Create(File => BME280_file,
                               Mode => Ada.Text_IO.Out_File,
                               Name => path & "BME280.csv");
            Ada.Text_IO.Create(File => CCS811_file,
                               Mode => Ada.Text_IO.Out_File,
                               Name => path & "CCS811.csv");
            Ada.Text_IO.Create(File => TSL2561_file,
                               Mode => Ada.Text_IO.Out_File,
                               Name => path & "TSL2561.csv");
            Ada.Text_IO.Put_Line(Info_file, "Time,Node,Addresses,Name");
            Ada.Text_IO.Put_Line(BME280_file, "Time,Node,Temperature,Pressure,Humidity");
            Ada.Text_IO.Put_Line(CCS811_file, "Time,Node,eCO2,TVOC");
            Ada.Text_IO.Put_Line(TSL2561_file, "Time,Node,Data0,Data1,LUX");
         when SQLite =>
            null;
         when others =>
            null;
         end case;
         selected_db := kind;
      end if;
   end;
   --
   --  Log the data record to the database.
   --
   procedure log(node : Integer; d : common.data_record) is
   begin
      case selected_db is
         when None =>
            null;
         when CSV =>
            log_csv(node, d);
         when SQLite =>
            log_sqlite(node, d);
         when others =>
            null;
      end case;
   end;
   --
   --  Cleanly close any log files
   --
   procedure end_log is
      temp : constant database_type := selected_db;
   begin
      selected_db := None;
      case temp is
         when None =>
            null;
         when CSV =>
            Ada.Text_IO.Close(Info_file);
            Ada.Text_IO.Close(BME280_file);
            Ada.Text_IO.Close(CCS811_file);
            Ada.Text_IO.Close(TSL2561_file);
         when SQLite =>
            null;
         when others =>
            null;
      end case;
   end;
   -- ----------------------------------
   -- Private procedures and functions
   --
--   type data_record (message : message_types := MSG_TYPE_UNKNOWN) is
--      record
--         validity : msg_validity; -- Validity code
--         aging : Ada.Calendar.Time; -- Time record was created.
--         case message is
--            when MSG_TYPE_INFO =>
--               num_addr : BBS.embed.uint32;
--               name : String(1..32);
--            when MSG_TYPE_BME280 =>
--               BME280_status : msg_validity;
--               BME280_age : BBS.embed.uint32;
--               BME280_temp_c : Float;
--               BME280_pressure_pa : Float;
--               BME280_humidity : Float;
--            when MSG_TYPE_DISCRETE =>
--               disc_type : BBS.embed.uint32;
--               disc_value : BBS.embed.uint32;
--            when MSG_TYPE_ANALOG =>
--               an_type : BBS.embed.uint32;
--               an_count : BBS.embed.uint32;
--               an_data : an_data_type;
--            when MSG_TYPE_VERSION =>
--               null;
--            when MSG_TYPE_CCS811 =>
--               CCS811_status : msg_validity;
--               CCS811_age : BBS.embed.uint32;
--               CCS811_eCO2 : BBS.embed.uint32;
--               CCS811_TVOC : BBS.embed.uint32;
--            when MSG_TYPE_TSL2561 =>
--               TSL2561_status : msg_validity;
--               TSL2561_age : BBS.embed.uint32;
--               TSL2561_data0 :  BBS.embed.uint32;
--               TSL2561_data1 :  BBS.embed.uint32;
--               TSL2561_lux :  BBS.embed.uint32;
--            when MSG_TYPE_PCA9685 =>
--               PCA9685_set : arr_16bool;
--               PCA9685_on  : arr_16uint12;
--               PCA9685_off : arr_16uint12;
--            when others =>
--               null;
--         end case;
--      end record;

   --
   --  Data and database specific data logging procedures.
   --
   procedure log_csv(node : Integer; d : common.data_record) is
      prefix : String := Ada.Calendar.Formatting.Image(Ada.Calendar.Clock) & "," &
        Integer'Image(node);
   begin
      case d.message is
         when common.MSG_TYPE_INFO =>
            if (enable_info.get) then
               Ada.Text_IO.Put_Line(Info_file, prefix & "," & Integer'Image(Integer(d.num_addr)) & "," & d.name);
            end if;
         when common.MSG_TYPE_BME280 =>
            if (enable_BME280.get) then
               Ada.Text_IO.Put_Line(BME280_file, prefix & "," & Float'Image(d.BME280_temp_c) & "," &
                                      Float'Image(d.BME280_pressure_pa) & "," &
                                      Float'Image(d.BME280_humidity));
            end if;
         when common.MSG_TYPE_CCS811 =>
            if (enable_BME280.get) then
               Ada.Text_IO.Put_Line(CCS811_file, prefix & "," & Integer'Image(Integer(d.CCS811_eCO2)) & "," &
                                      Integer'Image(Integer(d.CCS811_TVOC)));
            end if;
         when common.MSG_TYPE_TSL2561 =>
            if (enable_TSL2561.get) then
               Ada.Text_IO.Put_Line(TSL2561_file, prefix & "," & Integer'Image(Integer(d.TSL2561_data0)) & "," &
                                      Integer'Image(Integer(d.TSL2561_data1)) & "," & Integer'Image(Integer(d.TSL2561_lux)));
            end if;
         when others =>
            null;
      end case;
   end;
   --
   procedure log_sqlite(node : Integer; d : common.data_record) is
   begin
      null;
   end;
   --

end database;
