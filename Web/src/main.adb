with rs485;
with internal;
with Ada.Text_IO;
with BBS.web_server;
with BBS.web_common;
with BBS.svg;
with BBS.internal;

procedure Main is
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
--      internal_map.Insert("configure", internal.html_show_config'Access);
      internal_map.Insert("counter", internal.xml_count'Access);
      internal_map.Insert("debugging", internal.xml_debugging'Access);
      internal_map.Insert("devices", internal.html_devices'Access);
      internal_map.Insert("device-count", internal.xml_devices'Access);
      internal_map.Insert("device-data", internal.xml_device_data'Access);
      internal_map.Insert("name", internal.xml_device_name'Access);
      internal_map.Insert("send-command", internal.xml_send_command'Access);
   end;

begin
   rs485.state_machine.start;
   build_internal_map;
   bbs.web_server.server(internal_map, "config.txt", 31415);
end Main;
