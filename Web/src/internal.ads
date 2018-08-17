with Ada.Containers;
use type Ada.Containers.Count_Type;
with Ada.Float_Text_IO;
with Ada.Streams;
With Ada.Strings;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
use type Ada.Strings.Unbounded.Unbounded_String;
with Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO;
with GNAT.Sockets;
with BBS.embed;
use type BBS.embed.uint32;
with http;
with html;
with svg;
with web_common;
with rs485;
use type rs485.message_types;
--
-- This package contains the various internal routines to generate pages, data.
-- figures, and the like.  This is where the main customization will occure.
--
package internal is
   --
   -- Return the count of transactions as an xml message
   --
   procedure xml_count(s : GNAT.Sockets.Stream_Access);
   --
   -- Display the configuration data as a table.
   --
   procedure html_show_config(s : GNAT.Sockets.Stream_Access);
   --
   -- Display information sent in a form
   --
   procedure target(s : GNAT.Sockets.Stream_Access; p : web_common.params.Map);

   --
   -- Display table consisting of address 0 (device ID) for all devices
   --
   procedure html_devices(s : GNAT.Sockets.Stream_Access);

private
   CRLF : String renames web_common.CRLF;
   --
   -- Some helper functions - to be used only by other internal functions.  They
   -- won't work if called with the wrong variant of rs485.data_record.
   --
   --
   -- Display an info record as a table that can be nested in another table
   --
   procedure html_info(s : GNAT.Sockets.Stream_Access; d : rs485.data_record);
   --
   -- Display an BME280 record as a table that can be nested in another table.
   -- The metric boolean controls whether data is displayed in metric or standard.
   --
   procedure html_bme280(s : GNAT.Sockets.Stream_Access; d : rs485.data_record;
                        metric : Boolean);
   --
   -- Display an discrete record as a table that can be nested in another table
   --
   procedure html_discrete(s : GNAT.Sockets.Stream_Access; d : rs485.data_record);
   --
   -- Display an CCS811 record as a table that can be nested in another table
   --
   procedure html_ccs811(s : GNAT.Sockets.Stream_Access; d : rs485.data_record);
   --
   -- Display an TSL2561 record as a table that can be nested in another table
   --
   procedure html_tsl2561(s : GNAT.Sockets.Stream_Access; d : rs485.data_record);
end;
