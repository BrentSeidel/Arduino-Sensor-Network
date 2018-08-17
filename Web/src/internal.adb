package body internal is

   --
   -- Return the count of transactions as an xml message
   --
   procedure xml_count(s : GNAT.Sockets.Stream_Access) is
   begin
      http.ok(s, "application/xml");
      String'Write(s, "<xml><counter>" & Integer'Image(web_common.counter) &
                     "</counter></xml>" & CRLF);
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
   -- Display table consisting of address 0 (device ID) for all devices
   --
   procedure html_devices(s : GNAT.Sockets.Stream_Access) is
      t : rs485.data_record;
   begin
      http.ok(s, "text/html");
      html.html_head(s, "Device list", "Style");
      String'Write(s, "<table><tr><th>Device ID</th><th>Data</th></tr>" & CRLF);
      for i in Natural range 0 .. Integer(rs485.data_store.Length) - 1 loop
         String'Write(s, "<tr><td>" & Integer'Image(Integer(i)) & "</td>");
         String'Write(s, "<td><table class=""noborder"">" & CRLF);
         for j in Natural range 0 .. Integer(rs485.data_store(i).Length) - 1 loop
            t := rs485.data_store.Element(i).Element(j);
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
         if (rs485.data_store.Element(i).Length = 0) then
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
--      String'Write(s, "Discretes not yet implemented.");
      String'Write(s, "<table>" & CRLF);
      for i in  reverse 0 .. 3 loop
         String'Write(s, "<tr>");
         for j in  reverse 0 .. 7 loop
            if ((t and (2**(i*16#100# + j))) = 0) then
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

end;
