with Ada.Calendar;
use type Ada.Calendar.Time;
with Ada.Containers;
use type Ada.Containers.Count_Type;
with Ada.Float_Text_IO;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with BBS.embed;
use type BBS.embed.uint32;
with bbs.http;
with bbs.html;
with bbs.web_server;

package body internal is

   --
   --  Return various counters as an xml message.  The counters returned include:
   --    HTTP Transaction count
   --    Active HTTP tasks
   --    RS-485 Activity Counter
   --
   procedure xml_count(s : GNAT.Sockets.Stream_Access;
                       h : bbs.web_common.params.Map;
                       p : bbs.web_common.params.Map) is
   begin
      bbs.http.ok(s, "application/xml");
      String'Write(s, "<xml><counter>" & Integer'Image(bbs.web_common.request_counter.read) &
                     "</counter><tasks>" & Integer'Image(bbs.web_common.task_counter.read) &
                     "</tasks><rs485>" & Integer'Image(Integer(rs485.activity_counter)) &
                     "</rs485></xml>" & CRLF);
   end xml_count;

   --
   -- Display table consisting of data for all devices
   --
   procedure html_devices(s : GNAT.Sockets.Stream_Access;
                          h : bbs.web_common.params.Map;
                          p : bbs.web_common.params.Map) is
      t : rs485.data_record;
   begin
      bbs.http.ok(s, "text/html");
      bbs.html.html_head(s, "Device list", "Style");
      String'Write(s, "<table><tr><th>Device ID</th><th>Data</th></tr>" & CRLF);
      for i in Natural range 0 .. Integer(rs485.data_store.get_length) - 1 loop
         String'Write(s, "<tr><td>" & Integer'Image(Integer(i)) & "</td>");
         String'Write(s, "<td><table class=""noborder"">" & CRLF);
         for j in Natural range 0 .. Integer(rs485.data_store.get_length(i)) - 1 loop
            t := rs485.data_store.get_element(i, j);
            case t.message is
               when rs485.MSG_TYPE_INFO =>
                  String'Write(s, "<td class=""noborder"">");
                  html_info(s, t);
                  String'Write(s, "</td></tr>" & CRLF);
               when rs485.MSG_TYPE_BME280 =>
                  String'Write(s, "<td class=""noborder"">");
                  html_bme280(s, t, true);
                  String'Write(s, "</td></tr>" & CRLF);
               when rs485.MSG_TYPE_DISCRETE =>
                  String'Write(s, "<td class=""noborder"">");
                  html_discrete(s, t);
                  String'Write(s, "</td></tr>" & CRLF);
               when rs485.MSG_TYPE_CCS811 =>
                  String'Write(s, "<td class=""noborder"">");
                  html_ccs811(s, t);
                  String'Write(s, "</td></tr>" & CRLF);
               when rs485.MSG_TYPE_TSL2561 =>
                  String'Write(s, "<td class=""noborder"">");
                  html_TSL2561(s, t);
                  String'Write(s, "</td></tr>" & CRLF);
               when others =>
                  String'Write(s, "<td class=""error"">Unknown element type</td></tr>" & CRLF);
            end case;
         end loop;
         if (rs485.data_store.get_length(i) = 0) then
            String'Write(s, "<td class=""error"">No data</td></tr>" & CRLF);
         end if;
         String'Write(s, "</table></td>" & CRLF);
      end loop;
      String'Write(s, "</table>" & CRLF);
      bbs.html.html_end(s, "footer.html");
   end html_devices;

   --
   --  Display an info record as a table that can be nested in another table
   --
   procedure html_info(s : GNAT.Sockets.Stream_Access; d : rs485.data_record) is
   begin
      String'Write(s, "<table><tr><th>Addresses</th>" &
                     "<th>Device Name</th></tr>" & CRLF);
      String'Write(s, "<td>" & Integer'Image(Integer(d.num_addr)) & "</td>");
      String'Write(s, "<td>" & d.name & "</td></tr>" & CRLF);
      String'Write(s, "</table>" & CRLF);
   end html_info;

   --
   --  Display an BME280 record as a table that can be nested in another table
   --
   procedure html_bme280(s : GNAT.Sockets.Stream_Access; d : rs485.data_record;
                         metric : Boolean) is
      str : String := "     ";
   begin
      String'Write(s, "<table><tr><th>Temperature</th><th>Pressure</th>" &
                     "<th>Humidity</th></tr><tr>" & CRLF);
      if metric then
         Ada.Float_Text_IO.Put(To   => str,
                               Item => d.BME280_temp_c,
                               Aft  => 1,
                               Exp  => 0);
         String'Write(s, "<td>" & str & "&deg;C</td>");
         String'Write(s, "<td>" & Integer'Image(Integer(d.BME280_pressure_pa)) &
                        "Pa</td>");
      else
         Ada.Float_Text_IO.Put(To   => str,
                               Item => d.BME280_temp_c*9.0/5.0 + 32.0,
                               Aft  => 1,
                               Exp  => 0);
         String'Write(s, "<td>" & str & "&deg;F</td>");
         String'Write(s, "<td>" & Integer'Image(Integer(d.BME280_pressure_pa/3386.39)) &
                        "inHg</td>");
      end if;
      String'Write(s, "<td>" & Integer'Image(Integer(d.BME280_humidity)) &
                     "%</td>");
      String'Write(s, "</tr></table>" & CRLF);
   end html_bme280;
   --
   --  Display an discrete record as a table that can be nested in another table
   --
   procedure html_discrete(s : GNAT.Sockets.Stream_Access; d : rs485.data_record) is
      t : BBS.embed.uint32 := d.disc_value;
   begin
      String'Write(s, "<table>" & CRLF);
      for i in  reverse 0 .. 3 loop
         String'Write(s, "<tr>");
         for j in  reverse 0 .. 7 loop
            if ((t and (2**(i*8 + j))) = 0) then
               String'Write(s, "<td class=""clear"">0</td>");
            else
               String'Write(s, "<td class=""set"">1</td>");
            end if;
         end loop;
         String'Write(s, "</tr>" & CRLF);
      end loop;
      String'Write(s, "</table>" & CRLF);
   end html_discrete;
   --
   --  Display an CCS811 record as a table that can be nested in another table
   --
   procedure html_ccs811(s : GNAT.Sockets.Stream_Access; d : rs485.data_record) is
   begin
      String'Write(s, "<table><tr><th>eCO2</th><th>TVOC</th></tr><tr>" & CRLF);
      String'Write(s, "<td>" & Integer'Image(Integer(d.CCS811_eCO2)) & "</td>");
      String'Write(s, "<td>" & Integer'Image(Integer(d.CCS811_TVOC)) & "</td>");
      String'Write(s, "</tr></table>" & CRLF);
   end html_ccs811;
   --
   --  Display an TSL2561 record as a table that can be nested in another table
   --
   procedure html_tsl2561(s : GNAT.Sockets.Stream_Access; d : rs485.data_record) is
   begin
      String'Write(s, "<table><tr><th>Data 0</th><th>Data 1</th>" &
                   "<th>Lux</th></tr><tr>" & CRLF);
      String'Write(s, "<td>" & Integer'Image(Integer(d.TSL2561_data0)) & "</td>");
      String'Write(s, "<td>" & Integer'Image(Integer(d.TSL2561_data1)) & "</td>");
      String'Write(s, "<td>" & Integer'Image(Integer(d.TSL2561_lux)) & "</td>");
      String'Write(s, "</tr></table>" & CRLF);
   end html_tsl2561;

   --
   --  Provide data in XML format
   --
   --  Send length of data store
   --
   procedure xml_devices(s : GNAT.Sockets.Stream_Access;
                         h : bbs.web_common.params.Map;
                         p : bbs.web_common.params.Map) is
   begin
      bbs.http.ok(s, "application/xml");
      String'Write(s, "<xml><length>" & Integer'Image(Integer
                                                      (rs485.data_store.get_length)) &
                     "</length></xml>" & CRLF);
   end xml_devices;
   --
   --  Get device name
   --  Device is specfied by the parameter "device".
   --
   procedure xml_device_name(s : GNAT.Sockets.Stream_Access;
                             h : bbs.web_common.params.Map;
                             p : bbs.web_common.params.Map) is
      dev_id : Integer := 0;
      now : constant Ada.Calendar.Time := Ada.Calendar.Clock;
      t : rs485.data_record;
      rec : rs485.device_record;
   begin
      bbs.http.ok(s, "application/xml");
      if (bbs.web_common.params.Contains(p, "device")) then
         begin
            dev_id := Integer'Value(bbs.web_common.params.Element(p, "device"));
         exception
            when others =>
               dev_id := 0;
         end;
      end if;
      if (dev_id >= 0) and (dev_id < Integer(rs485.data_store.get_length)) then
         if (rs485.data_store.get_length(dev_id) > 0) then
            t := rs485.data_store.get_element(dev_id, 0);
            rec := rs485.data_store.get_device(dev_id);
            String'Write(s, "<xml>");
            case t.message is
               when rs485.MSG_TYPE_INFO =>
                  String'Write(s, "<info><validity>" & rs485.msg_validity'Image(t.validity) &
                                 "</validity><aging>" & Duration'Image(now - rec.info_age) &
                                 "</aging><presence>" & Duration'Image(now - rec.last_age) &
                                 "</presence><addresses>" & Integer'Image(Integer(rec.num_addr)) &
                                 "</addresses><name>" & rec.name & "</name></info>");
               when others =>
                  String'Write(s, "<error>Unknown record type</error>");
            end case;
            String'Write(s, "</xml>" & CRLF);
         else
            String'Write(s, "<xml><error>No data for device " & Integer'Image(dev_id) &
                           "</error></xml>" & CRLF);
         end if;
      else
         String'Write(s, "<xml><error>No data for device " & Integer'Image(dev_id) &
                        "</error></xml>" & CRLF);
      end if;
   end xml_device_name;
   --
   --  Get device data - returns all data for a device in XML
   --  Device is specfied by the parameter "device".
   --
   procedure xml_device_data(s : GNAT.Sockets.Stream_Access;
                             h : bbs.web_common.params.Map;
                             p : bbs.web_common.params.Map) is
      dev_id : Integer := 0;
      recs : Integer;
      t : rs485.data_record;
   begin
      bbs.http.ok(s, "application/xml");
      if (bbs.web_common.params.Contains(p, "device")) then
         begin
            dev_id := Integer'Value(bbs.web_common.params.Element(p, "device"));
         exception
            when others =>
               dev_id := 0;
         end;
      end if;
      if (dev_id >= 0) and (dev_id < Integer(rs485.data_store.get_length)) then
         recs := Integer(rs485.data_store.get_length(dev_id));
         if (recs > 0) then
            String'Write(s, "<xml>");
            for i in Natural range 0 .. Natural(recs) - 1 loop
               t := rs485.data_store.get_element(dev_id, i);
               case t.message is
                  when rs485.MSG_TYPE_INFO =>
                     xml_info_msg(s, t);
                  when rs485.MSG_TYPE_BME280 =>
                     xml_bme280_msg(s, t);
                  when rs485.MSG_TYPE_DISCRETE =>
                     xml_discrete_msg(s, t);
                  when rs485.MSG_TYPE_ANALOG =>
                     xml_analog_msg(s, t);
                  when rs485.MSG_TYPE_CCS811 =>
                     xml_ccs811_msg(s, t);
                  when rs485.MSG_TYPE_TSL2561 =>
                     xml_tsl2561_msg(s, t);
                  when others =>
                     String'Write(s, "<error>Unknown record type</error>");
               end case;
            end loop;
            String'Write(s, "</xml>" & CRLF);
         else
            String'Write(s, "<xml><error>No data for device " & Integer'Image(dev_id) &
                           "</error></xml>" & CRLF);
         end if;
      else
         String'Write(s, "<xml><error>No data for device " & Integer'Image(dev_id) &
                        "</error></xml>" & CRLF);
      end if;
   end xml_device_data;
   --
   --  Provide data in XML format
   --
   --  Provide an XML version of the info message
   --
   procedure xml_info_msg(s : GNAT.Sockets.Stream_Access; d : rs485.data_record) is
      now : constant Ada.Calendar.Time := Ada.Calendar.Clock;
   begin
      String'Write(s, "<info><validity>" & rs485.msg_validity'Image(d.validity) &
                     "</validity><aging>" & Duration'Image(now - d.aging) &
                     "</aging><addresses>" & Integer'Image(Integer(d.num_addr)) &
                     "</addresses><name>" & d.name & "</name></info>");
   end xml_info_msg;
   --
   --  Provide an XML version of the BME280 message
   --
   procedure xml_BME280_msg(s : GNAT.Sockets.Stream_Access; d : rs485.data_record) is
      now : constant Ada.Calendar.Time := Ada.Calendar.Clock;
   begin
      String'Write(s, "<bme280><validity>" & rs485.msg_validity'Image(d.validity) &
                     "</validity><aging>" & Duration'Image(now - d.aging) &
                     "</aging><bme280_status>" & rs485.msg_validity'Image(d.BME280_status) &
                     "</bme280_status><bme280_age>" & Integer'Image(Integer(d.BME280_age)) &
                     "</bme280_age><bme280_temp_c>" & Float'Image(d.BME280_temp_c) &
                     "</bme280_temp_c><bme280_pressure_pa>" & Float'Image(d.BME280_pressure_pa) &
                     "</bme280_pressure_pa><bme280_humidity>" & Float'Image(d.BME280_humidity) &
                     "</bme280_humidity></bme280>");
   end xml_BME280_msg;
   --
   --  Provide an XML version of the discrete message
   --
   procedure xml_discrete_msg(s : GNAT.Sockets.Stream_Access; d : rs485.data_record) is
      now : constant Ada.Calendar.Time := Ada.Calendar.Clock;
   begin
      String'Write(s, "<discrete><validity>" & rs485.msg_validity'Image(d.validity) &
                     "</validity><aging>" & Duration'Image(now - d.aging) &
                     "</aging><disc_type>" & Integer'Image(Integer(d.disc_type)) &
                     "</disc_type><disc_value>" & Integer'Image(Integer(d.disc_value)) &
                     "</disc_value></discrete>");
   end xml_discrete_msg;
   --
   --  Provide an XML version of the analog message
   --
   procedure xml_analog_msg(s : GNAT.Sockets.Stream_Access; d : rs485.data_record) is
      now : constant Ada.Calendar.Time := Ada.Calendar.Clock;
   begin
      String'Write(s, "<analogs><validity>" & rs485.msg_validity'Image(d.validity) &
                     "</validity><aging>" & Duration'Image(now - d.aging) &
                     "</aging><analog_type>" & Integer'Image(Integer(d.an_type)) &
                     "</analog_type><analog_count>" & Integer'Image(Integer(d.an_count)) &
                     "</analog_count>");
      for i in Integer range 1 .. Integer(d.an_count) loop
         String'Write(s, "<value>" & Integer'Image(Integer(d.an_data(i))) & "</value>");
      end loop;
      String'Write(s, "</analogs>");
   end xml_analog_msg;
   --
   --  Provide an XML version of the CCS811 message
   --
   procedure xml_ccs811_msg(s : GNAT.Sockets.Stream_Access; d : rs485.data_record) is
      now : constant Ada.Calendar.Time := Ada.Calendar.Clock;
   begin
      String'Write(s, "<ccs811><validity>" & rs485.msg_validity'Image(d.validity) &
                     "</validity><aging>" & Duration'Image(now - d.aging) &
                     "</aging><ccs811_status>" & rs485.msg_validity'Image(d.CCS811_status) &
                     "</ccs811_status><ccs811_age>" & Integer'Image(Integer(d.CCS811_age)) &
                     "</ccs811_age><ccs811_eco2>" & Integer'Image(Integer(d.CCS811_eCO2)) &
                     "</ccs811_eco2><ccs811_tvoc>" & Integer'Image(Integer(d.CCS811_TVOC)) &
                     "</ccs811_tvoc></ccs811>");
   end xml_ccs811_msg;
   --
   --  Provide an XML version of the TSL2561 message
   --
   procedure xml_tsl2561_msg(s : GNAT.Sockets.Stream_Access; d : rs485.data_record) is
      now : constant Ada.Calendar.Time := Ada.Calendar.Clock;
   begin
      null;
      String'Write(s, "<tsl2561><validity>" & rs485.msg_validity'Image(d.validity) &
                     "</validity><aging>" & Duration'Image(now - d.aging) &
                     "</aging><tsl2561_status>" & rs485.msg_validity'Image(d.TSL2561_status) &
                     "</tsl2561_status><tsl2561_age>" & Integer'Image(Integer(d.TSL2561_age)) &
                     "</tsl2561_age><tsl2561_data0>" & Integer'Image(Integer(d.TSL2561_data0)) &
                     "</tsl2561_data0><tsl2561_data1>" & Integer'Image(Integer(d.TSL2561_data1)) &
                     "</tsl2561_data1><tsl2561_lux>" & Integer'Image(Integer(d.TSL2561_lux)) &
                     "</tsl2561_lux></tsl2561>");
   end xml_tsl2561_msg;
   --
   --  Request to send a command.
   --
   procedure xml_send_command(s : GNAT.Sockets.Stream_Access;
                              h : bbs.web_common.params.Map;
                              p : bbs.web_common.params.Map) is
   begin
      bbs.http.ok(s, "application/xml");
      if (bbs.web_common.params.Contains(p, "command")) then
         declare
            cmd : constant String := bbs.web_common.params.Element(p, "command");
         begin
            rs485.command_task.send_cmd(cmd);
            String'Write(s, "<xml><command>" & cmd & "</command></xml>" & CRLF);
         end;
      else
         String'Write(s, "<xml><error>No command supplied</error></xml>" & CRLF);
      end if;
   end xml_send_command;
   --
   --  Set and retrieve debugging flags
   --
   procedure xml_debugging(s : GNAT.Sockets.Stream_Access;
                           h : bbs.web_common.params.Map;
                           p : bbs.web_common.params.Map) is
   begin
      bbs.http.ok(s, "application/xml");
      --
      -- Check to see which flags to change
      --
      if (bbs.web_common.params.Contains(p, "rs485.char")) then
         declare
            cmd : constant String := bbs.web_common.params.Element(p, "rs485.char");
         begin
            if (cmd(1) = 'T' or cmd(1) = 't') then
               rs485.debug_char.set;
            else
               rs485.debug_char.clear;
            end if;
         end;
      end if;
      if (bbs.web_common.params.Contains(p, "rs485.msg")) then
         declare
            cmd : constant String := bbs.web_common.params.Element(p, "rs485.msg");
         begin
            if (cmd(1) = 'T' or cmd(1) = 't') then
               rs485.debug_msg.set;
            else
               rs485.debug_msg.clear;
            end if;
         end;
      end if;
      if (bbs.web_common.params.Contains(p, "http.head")) then
         declare
            cmd : constant String := bbs.web_common.params.Element(p, "http.head");
         begin
            if (cmd(1) = 'T' or cmd(1) = 't') then
               bbs.http.debug_head.set;
            else
               bbs.http.debug_head.clear;
            end if;
         end;
      end if;
      if (bbs.web_common.params.Contains(p, "http.msg")) then
         declare
            cmd : constant String := bbs.web_common.params.Element(p, "http.msg");
         begin
            if (cmd(1) = 'T' or cmd(1) = 't') then
               bbs.http.debug_req.set;
            else
               bbs.http.debug_req.clear;
            end if;
         end;
      end if;
      if (bbs.web_common.params.Contains(p, "web.dbg")) then
         declare
            cmd : constant String := bbs.web_common.params.Element(p, "web.dbg");
         begin
            if (cmd(1) = 'T' or cmd(1) = 't') then
              bbs.web_server.debug.set;
            else
               bbs.web_server.debug.clear;
            end if;
         end;
      end if;
      --
      --  Build XML reply giving state of all debug flags.
      --
      String'Write(s, "<xml><rs485.char>" & Boolean'Image(rs485.debug_char.get) &
                     "</rs485.char><rs485.msg>" & Boolean'Image(rs485.debug_msg.get) &
                     "</rs485.msg><http.head>" & Boolean'Image(bbs.http.debug_head.get) &
                     "</http.head><http.msg>" & Boolean'Image(bbs.http.debug_req.get) &
                     "</http.msg><web.dbg>" & Boolean'Image(bbs.web_server.debug.get) &
                     "</web.dbg></xml>" & CRLF);
   end xml_debugging;

end;
