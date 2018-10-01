with rs485;
with internal;
with Ada.Text_IO;
with Ada.Exceptions;
with BBS.web_server;
with BBS.web_common;
with BBS.svg;
with BBS.internal;

procedure Main is
--   worked : Boolean;
   internal_map : bbs.web_common.proc_tables.Map;
   --
   --  Build the map for internal procedure calls.  The key strings must match
   --  the identifications in the configuration file.  The generated map is
   --  used by both the GET and POST methods.
   --
   procedure build_internal_map is
   begin
      --
      --  ******************************************************
      --  Customization goes here to add any internally routines
      --  to generate responses.
      --
      internal_map.Insert("reload", bbs.internal.html_reload_config'Access);
      internal_map.Insert("target", bbs.internal.target'Access);
      --
      internal_map.Insert("dial", bbs.svg.dial'Access);
      internal_map.Insert("thermometer", bbs.svg.thermometer'Access);
      --
      internal_map.Insert("counter", internal.xml_count'Access);
      internal_map.Insert("debugging", internal.xml_debugging'Access);
      internal_map.Insert("devices", internal.html_devices'Access);
      internal_map.Insert("device-count", internal.xml_devices'Access);
      internal_map.Insert("device-data", internal.xml_device_data'Access);
      internal_map.Insert("name", internal.xml_device_name'Access);
      internal_map.Insert("send-command", internal.xml_send_command'Access);
   end;

begin
   --
   --  Warning: The command port and the data read port are the same physical device.  So, the same file is
   --           being opened in two separate threads: one as an input file and the other as an output file.
   --           There seems to be some sensitivity here where if things aren't done just right, one of the
   --           opens will fail to work properly.  I haven't yet been able to track down the exact cause,
   --           but I suspect a timing problem with the two opens.  Adding some task barriers to force them
   --           to occur sequentually may help.
   --
   rs485.rs485_cmd_type.start;
   rs485.state_machine.start;
   --
   --  Once the RS-485 tasks are started, build the map for the web server and start it.  It should only exit
   --  if an exception occurs during processing.
   --
   build_internal_map;
   bbs.web_server.server(internal_map, "config.txt", 31415);
   Ada.Text_IO.Put_Line("Stopping RS-485 command task.");
   --
   --  Try to shut down the RS-485 tasks.
   --
   select
      rs485.rs485_cmd_type.stop;
   or
      delay 1.0;
   end select;
exception
   when err: Others =>
      Ada.Text_IO.Put_Line("Unhandled exception occured during operation.");
      Ada.Text_IO.Put_Line(Ada.Exceptions.Exception_Information(err));
      select
         rs485.rs485_cmd_type.stop;
      or
         delay 1.0;
      end select;
end Main;
