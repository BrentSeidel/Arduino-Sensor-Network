with Ada.Calendar;
use type Ada.Calendar.Time;
with Ada.Containers;
use type Ada.Containers.Count_Type;
with Ada.Float_Text_IO;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with BBS.embed;
use type BBS.embed.uint32;
with http;
with html;
with web_server;
--with svg;

package body internal is

   --
   -- Return various counters as an xml message.  The counters returned include:
   -- HTTP Transaction count
   -- Active HTTP tasks
   -- RS-485 Activity Counter
   --
   procedure xml_count(s : GNAT.Sockets.Stream_Access) is
   begin
      http.ok(s, "application/xml");
      String'Write(s, "<xml><counter>" & Integer'Image(web_common.counter) &
                     "</counter><tasks>" & Integer'Image(web_common.task_counter.read) &
                     "</tasks><rs485>" & Integer'Image(Integer(rs485.activity_counter)) &
                     "</rs485></xml>" & CRLF);
   end;

   --
   -- Display the configuration data as a table.
   --
   procedure html_show_config(s : GNAT.Sockets.Stream_Access) is
      index : web_common.dictionary.Cursor := web_common.dictionary.First(web_common.directory);
      el : web_common.element;
   begin
      http.ok(s, "text/html");
      html.html_head(s, "Page Condfiguration List", "Style");
      String'Write(s, "<p>Table showing the page configuration list</p>" & CRLF);
      String'Write(s, "<table>" & CRLF);
      String'Write(s, "<tr><th>Key</th><th>File Name</th><th>MIME Type</th></tr></tr>" & CRLF);
      --
      -- Start table and populate it by iterating over directory
      --
      while (web_common.dictionary.Has_Element(index)) loop
         el := web_common.dictionary.Element(index);
         String'Write(s, "<tr><td>" & web_common.dictionary.Key(index) & "</td><td>" &
                        Ada.Strings.Unbounded.To_String(el.file) & "</td><td>" &
                        Ada.Strings.Unbounded.To_String(el.mime) & "</td></tr>" & CRLF);
         web_common.dictionary.Next(index);
      end loop;
      String'Write(s, "</table>" & CRLF);
      html.html_end(s, "footer.html");
   end;
   --
   -- Display information sent in a form
   --
   procedure target(s : GNAT.Sockets.Stream_Access; p : web_common.params.Map) is
      index : web_common.params.Cursor := web_common.params.First(p);
   begin
      http.ok(s, "text/html");
      html.html_head(s, "Form Parameters", "Style");
      String'Write(s, "<p>Table showing the parameters submitted in a form</p>" & CRLF);
      String'Write(s, "<table>" & CRLF);
      String'Write(s, "<tr><th>Key</th><th>Value</th></tr></tr>" & CRLF);
      --
      -- Start table and populate it by iterating over directory
      --
      while (web_common.params.Has_Element(index)) loop
         String'Write(s, "<tr><td>" & web_common.params.Key(index) & "</td><td>" &
                        web_common.params.Element(index) & "</td></tr>" & CRLF);
         web_common.params.Next(index);
      end loop;
      String'Write(s, "</table>" & CRLF);
      html.html_end(s, "footer.html");
   end;
   --
   -- Request that the configuration file be reloaded.
   --
   procedure html_reload_config(s : GNAT.Sockets.Stream_Access) is
   begin
      http.ok(s, "text/html");
      html.html_head(s, "Reload Requested", "Style");
      String'Write(s, "<h1>Reload Request</h1>");
      String'Write(s, "<p>Configuration file reload request submitted.</p>" & CRLF);
      html.html_end(s, "footer.html");
   end;

   --
   -- Display table consisting of data for all devices
   --
   procedure html_devices(s : GNAT.Sockets.Stream_Access) is
      t : rs485.data_record;
   begin
      http.ok(s, "text/html");
      html.html_head(s, "Device list", "Style");
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
      html.html_end(s, "footer.html");
   end;

   --
   -- Display an info record as a table that can be nested in another table
   --
   procedure html_info(s : GNAT.Sockets.Stream_Access; d : rs485.data_record) is
   begin
      String'Write(s, "<table><tr><th>Addresses</th>" &
                     "<th>Device Name</th></tr>" & CRLF);
      String'Write(s, "<td>" & Integer'Image(Integer(d.num_addr)) & "</td>");
      String'Write(s, "<td>" & d.name & "</td></tr>" & CRLF);
      String'Write(s, "</table>" & CRLF);
   end;

   --
   -- Display an BME280 record as a table that can be nested in another table
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
   end;

   --
   -- Display an discrete record as a table that can be nested in another table
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
   end;

   --
   -- Display an CCS811 record as a table that can be nested in another table
   --
   procedure html_ccs811(s : GNAT.Sockets.Stream_Access; d : rs485.data_record) is
   begin
      String'Write(s, "<table><tr><th>eCO2</th><th>TVOC</th></tr><tr>" & CRLF);
      String'Write(s, "<td>" & Integer'Image(Integer(d.CCS811_eCO2)) & "</td>");
      String'Write(s, "<td>" & Integer'Image(Integer(d.CCS811_TVOC)) & "</td>");
      String'Write(s, "</tr></table>" & CRLF);
   end;

   --
   -- Display an TSL2561 record as a table that can be nested in another table
   --
   procedure html_tsl2561(s : GNAT.Sockets.Stream_Access; d : rs485.data_record) is
   begin
      String'Write(s, "<table><tr><th>Data 0</th><th>Data 1</th>" &
                   "<th>Lux</th></tr><tr>" & CRLF);
      String'Write(s, "<td>" & Integer'Image(Integer(d.TSL2561_data0)) & "</td>");
      String'Write(s, "<td>" & Integer'Image(Integer(d.TSL2561_data1)) & "</td>");
      String'Write(s, "<td>" & Integer'Image(Integer(d.TSL2561_lux)) & "</td>");
      String'Write(s, "</tr></table>" & CRLF);
   end;

   --
   -- Provide data in XML format
   --
   -- Send length of data store
   --
   procedure xml_devices(s : GNAT.Sockets.Stream_Access) is
   begin
      http.ok(s, "application/xml");
      String'Write(s, "<xml><length>" & Integer'Image(Integer
                                                      (rs485.data_store.get_length)) &
                     "</length></xml>" & CRLF);
   end;

   --
   -- Get device name
   -- Device is specfied by the parameter "device".
   --
   procedure xml_device_name(s : GNAT.Sockets.Stream_Access; p : web_common.params.Map) is
      dev_id : Integer := 0;
      t : rs485.data_record;
   begin
      http.ok(s, "application/xml");
      if (web_common.params.Contains(p, "device")) then
         begin
            dev_id := Integer'Value(web_common.params.Element(p, "device"));
         exception
            when others =>
               dev_id := 0;
         end;
      end if;
      if (dev_id >= 0) and (dev_id < Integer(rs485.data_store.get_length)) then
         if (rs485.data_store.get_length(dev_id) > 0) then
            t := rs485.data_store.get_element(dev_id, 0);
            String'Write(s, "<xml>");
            case t.message is
               when rs485.MSG_TYPE_INFO =>
                  xml_info_msg(s, t);
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
   end;

   --
   -- Get device data - returns all data for a device in XML
   -- Device is specfied by the parameter "device".
   --
   procedure xml_device_data(s : GNAT.Sockets.Stream_Access; p : web_common.params.Map) is
      dev_id : Integer := 0;
      recs : Integer;
      t : rs485.data_record;
   begin
      http.ok(s, "application/xml");
      if (web_common.params.Contains(p, "device")) then
         begin
            dev_id := Integer'Value(web_common.params.Element(p, "device"));
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
   end;

   --
   -- Provide data in XML format
   --
   -- Provide an XML version of the info message
   --
   procedure xml_info_msg(s : GNAT.Sockets.Stream_Access; d : rs485.data_record) is
      now : constant Ada.Calendar.Time := Ada.Calendar.Clock;
   begin
      String'Write(s, "<info><validity>" & rs485.msg_validity'Image(d.validity) &
                     "</validity><aging>" & Duration'Image(now - d.aging) &
                     "</aging><addresses>" & Integer'Image(Integer(d.num_addr)) &
                     "</addresses><name>" & d.name & "</name></info>");
   end;
   --
   -- Provide an XML version of the BME280 message
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
   end;
   --
   -- Provide an XML version of the discrete message
   --
   procedure xml_discrete_msg(s : GNAT.Sockets.Stream_Access; d : rs485.data_record) is
      now : constant Ada.Calendar.Time := Ada.Calendar.Clock;
   begin
      String'Write(s, "<discrete><validity>" & rs485.msg_validity'Image(d.validity) &
                     "</validity><aging>" & Duration'Image(now - d.aging) &
                     "</aging><disc_type>" & Integer'Image(Integer(d.disc_type)) &
                     "</disc_type><disc_value>" & Integer'Image(Integer(d.disc_value)) &
                     "</disc_value></discrete>");
   end;
   --
   -- Provide an XML version of the CCS811 message
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
   end;
   --
   -- Provide an XML version of the TSL2561 message
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
   end;
   --
   -- Request to send a command.
   --
   procedure xml_send_command(s : GNAT.Sockets.Stream_Access; p : web_common.params.Map) is
   begin
      http.ok(s, "application/xml");
      if (web_common.params.Contains(p, "command")) then
         declare
            cmd : constant String := web_common.params.Element(p, "command");
         begin
            rs485.rs485_cmd_type.send_cmd(cmd);
            String'Write(s, "<xml><command>" & cmd & "</command></xml>" & CRLF);
         end;
      else
         String'Write(s, "<xml><error>No command supplied</error></xml>" & CRLF);
      end if;
   end;
   --
   -- Set and retrieve debugging flags
   --
   procedure xml_debugging(s : GNAT.Sockets.Stream_Access; p : web_common.params.Map) is
   begin
      http.ok(s, "application/xml");
      --
      -- Check to see which flags to change
      --
      if (web_common.params.Contains(p, "rs485.char")) then
         declare
            cmd : constant String := web_common.params.Element(p, "rs485.char");
         begin
            if (cmd(1) = 'T' or cmd(1) = 't') then
               rs485.set_debug_char(True);
            else
               rs485.set_debug_char(False);
            end if;
         end;
      end if;
      if (web_common.params.Contains(p, "rs485.msg")) then
         declare
            cmd : constant String := web_common.params.Element(p, "rs485.msg");
         begin
            if (cmd(1) = 'T' or cmd(1) = 't') then
               rs485.set_debug_msg(True);
            else
               rs485.set_debug_msg(False);
            end if;
         end;
      end if;
      if (web_common.params.Contains(p, "http.head")) then
         declare
            cmd : constant String := web_common.params.Element(p, "http.head");
         begin
            if (cmd(1) = 'T' or cmd(1) = 't') then
               http.set_debug_head(True);
            else
               http.set_debug_head(False);
            end if;
         end;
      end if;
      if (web_common.params.Contains(p, "http.msg")) then
         declare
            cmd : constant String := web_common.params.Element(p, "http.msg");
         begin
            if (cmd(1) = 'T' or cmd(1) = 't') then
               http.set_debug_req(True);
            else
               http.set_debug_req(False);
            end if;
         end;
      end if;
      if (web_common.params.Contains(p, "web.dbg")) then
         declare
            cmd : constant String := web_common.params.Element(p, "web.dbg");
         begin
            if (cmd(1) = 'T' or cmd(1) = 't') then
               web_server.set_debug(True);
            else
               web_server.set_debug(False);
            end if;
         end;
      end if;
      --
      -- Build XML reply giving state of all debug flags.
      --
      String'Write(s, "<xml><rs485.char>" & Boolean'Image(rs485.get_debug_char) &
                     "</rs485.char><rs485.msg>" & Boolean'Image(rs485.get_debug_msg) &
                     "</rs485.msg><http.head>" & Boolean'Image(http.get_debug_head) &
                     "</http.head><http.msg>" & Boolean'Image(http.get_debug_req) &
                     "</http.msg><web.dbg>" & Boolean'Image(web_server.get_debug) &
                     "</web.dbg></xml>" & CRLF);
   end;

end;
