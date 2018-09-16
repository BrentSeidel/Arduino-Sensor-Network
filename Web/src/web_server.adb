with Ada.Text_IO;
with Ada.Strings.Unbounded;
with GNAT.Sockets;
with http;
use type http.request_type;
with text;
with binary;
with internal;
with svg;
with web_common;

package body web_server is

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
      web_common.internal_map.Insert("counter", internal.xml_count'Access);
      web_common.internal_map.Insert("configure", internal.html_show_config'Access);
      web_common.internal_map.Insert("target", internal.target'Access);
      web_common.internal_map.Insert("thermometer", svg.thermometer'Access);
      web_common.internal_map.Insert("dial", svg.dial'Access);
      web_common.internal_map.Insert("devices", internal.html_devices'Access);
      web_common.internal_map.Insert("device-count", internal.xml_devices'Access);
      web_common.internal_map.Insert("name", internal.xml_device_name'Access);
      web_common.internal_map.Insert("device-data", internal.xml_device_data'Access);
      web_common.internal_map.Insert("reload", internal.html_reload_config'Access);
      web_common.internal_map.Insert("send-command", internal.xml_send_command'Access);
      web_common.internal_map.Insert("debugging", internal.xml_debugging'Access);
   end build_internal_map;
   --
   --  This is the web server.  In initializes the network interface and enters
   --  an infinite loop processing requests.
   --
   procedure server is
      local : GNAT.Sockets.Sock_Addr_Type;
      server : GNAT.Sockets.Socket_Type;
      socket : GNAT.Sockets.Socket_Type;
      handlers : array (1 .. num_handlers) of request_handler;
      handler_index : Natural := 1;
   begin
      web_common.load_directory("config.txt");
      build_internal_map;
      --
      --  Do a bunch of initialization stuff.  We want to listen on any interface
      --  to the specified port.  The socket is IPv4 TCP/IP.
      --
      local.Addr := GNAT.Sockets.Any_Inet_Addr;
      local.port := web_common.port;
      GNAT.Sockets.Create_Socket(server, GNAT.Sockets.Family_Inet,
                                 GNAT.Sockets.Socket_Stream);
      GNAT.Sockets.Set_Socket_Option(server, GNAT.Sockets.Socket_Level,
                                     (GNAT.Sockets.Reuse_Address, True));
      GNAT.Sockets.Bind_Socket(server, local);
      --
      --  Once the socket is configured.  Listen on it and accept a connection.
      --  once the connection is made, read from it and write back a response.
      --  Then close the sockets and exit.
      --
      GNAT.Sockets.Listen_Socket(server);
      loop
         if (web_common.reload_configuration) then
            web_common.load_directory("config.txt");
            web_common.reload_configuration := False;
         end if;
         GNAT.Sockets.Accept_Socket(server, socket, local);
         web_common.counter := web_common.counter + 1;
         --
         --  Handlers contains a array of num_handlers tasks.  As requests come
         --  in, they are assigned to tasks in round-robin fashon.  This means
         --  that it is possible for a task that takes a long time to process to
         --  eventually delay serving of other tasks.  With a little more
         --  complexity, it would be possible to change this to use the next
         --  available task.  In most cases, tasks should complete quickly and
         --  this should not be a big problem.
         --
         if (debug) then
            Ada.Text_IO.Put_Line(Integer'Image(web_common.counter) & " requests serviced, " &
                                   Integer'Image(web_common.task_counter.read) &
                                   " active tasks.");
            Ada.Text_IO.Put_Line("Using server index " & Natural'Image(handler_index));
         end if;
         handlers(handler_index).start(socket);
         handler_index := handler_index + 1;
         if (handler_index > num_handlers) then
            handler_index := 1;
         end if;
      end loop;
      --
      --  Close the sockets and exit.  This will be done when some sort of shutdown
      --  signal is received to exit the loop above.  This is not yet implemented.
      --
--      GNAT.Sockets.Close_Socket(server);
--      Ada.Text_IO.Put_Line("Done.");
   end server;
   --
   --  Handle the details of the http request.  This is done as a task.  Once a
   --  network connection is made, the stream for that connection is handed off
   --  to a task which processes the request, runs to completion, and then exits.
   --
   task body request_handler is
      item : Ada.Strings.Unbounded.Unbounded_String;
      req : http.request_type;
      el : web_common.element;
      param : web_common.params.Map;
      headers : web_common.params.Map;
      s : GNAT.Sockets.Stream_Access;
      sock : GNAT.Sockets.Socket_Type;
      exit_flag : Boolean := False;
   begin
      loop
         select
            accept start(socket : GNAT.Sockets.Socket_Type) do
               sock := socket;
            end start;
         or
            accept end_task  do
               exit_flag := True;
            end end_task;
         end select;
         exit when exit_flag;
         web_common.task_counter.increment;
         s := GNAT.Sockets.Stream(sock);
         --
         --  First read the HTTP headers.  These will need to be parsed to
         --  determine what is being requested.  Both GET and POST requests
         --  should be handled.  POST requests will need to be able to handle
         --  passed parameters so that forms can be processed.
         --
         param.Clear;
         headers.Clear;
         http.read_headers(s, req, item, headers, param);
         --
         --  Check the request type.  If the type is Other, a request type not
         --  implemented response has already been sent.
         --
         case req is
            when http.GET =>
               --
               --  Check if the requested item is in the directory.
               --
               if (web_common.directory.Contains(Ada.Strings.Unbounded.To_String(item))) then
                  el := web_common.directory.Element(Ada.Strings.Unbounded.To_String(item));
                  declare
                     name : constant String := Ada.Strings.Unbounded.To_String(el.file);
                     mime : constant String := Ada.Strings.Unbounded.To_String(el.mime);
                  begin
                     --
                     --  The following mime types should be supported:
                     --  * application/javascript
                     --  * application/xml
                     --  * image/jpeg
                     --  * image/png
                     --  * image/svg+xml
                     --  * text/css
                     --  * text/html
                     --  * text/plain
                     --
                     --  Note that a pseudo type "internal" is also supported.
                     --  This indicates that the item is procedurally generated
                     --  and the procedure will be responsible for generating
                     --  the proper types.  Dispatch to the proper procedure
                     --  will be done based on the requested item.
                     --
                     if (mime = "text/html") or (mime = "text/plain") or
                       (mime = "text/css") or (mime = "application/javascript") or
                       (mime = "application/xml") or (mime = "image/svg+xml") then
                        --
                        --  Send an text type file with the proper mime type.
                        --
                        text.send_file_with_headers(s, mime, name);
                     elsif (mime = "image/jpeg") or (mime = "image/png") then
                        --
                        --  Send a binary file with the proper mime type.
                        --
                        binary.send_file_with_headers(s, mime, name);
                     elsif (mime = "internal") then
                        if web_common.internal_map.Contains(name) then
                           web_common.internal_map.Element(name)(s, headers, param);
                        end if;
                     else
                        --
                        --  If the mime type is unrecognized, it is an internal
                        --  error.
                        --
                        http.internal_error(s, mime);
                     end if;
                  end;
               else
                  http.not_found(s, Ada.Strings.Unbounded.To_String(item));
               end if;
            when http.POST =>
               --
               --  Post requests will only work on internal type files, so
               --  check if the requested item is in the directory and has a
               --  mime type of 'internal'.
               --
               if (web_common.directory.Contains(Ada.Strings.Unbounded.To_String(item))) then
                  el := web_common.directory.Element(Ada.Strings.Unbounded.To_String(item));
                  declare
                     name : constant String := Ada.Strings.Unbounded.To_String(el.file);
                     mime : constant String := Ada.Strings.Unbounded.To_String(el.mime);
                  begin
                     if (mime = "internal") then
                        if web_common.internal_map.Contains(name) then
                           web_common.internal_map.Element(name)(s, headers, param);
                        end if;
                     else
                        --
                        --  If the mime type is not 'internal', it is an
                        --  internal error.
                        --
                        http.internal_error(s, mime);
                     end if;
                  end;
               else
                  http.not_found(s, Ada.Strings.Unbounded.To_String(item));
               end if;
            when others => -- Handled in HTTP package
               null;
         end case;
         --
         --  Close the session socket
         --
         GNAT.Sockets.Close_Socket(sock);
         web_common.task_counter.decrement;
      end loop;
   end request_handler;

   --
   --  Set and get value of debug flag;
   --
   function get_debug return Boolean is
   begin
      return debug;
   end get_debug;
   --
   procedure set_debug(f : Boolean) is
   begin
      debug := f;
   end set_debug;

end web_server;
