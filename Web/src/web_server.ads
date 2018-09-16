with GNAT.Sockets;
with web_common;

package web_server is
   --
   -- This is the web server.  In initializes the network interface and enters
   -- an infinite loop processing requests.
   --
   procedure server;
   --
   -- Simple procedure to decode internally generated pages.  It's used by both
   -- GET and POST methods and so should be common.
   --
--   procedure decode_internal(s : GNAT.Sockets.Stream_Access; name : String;
--                             p : web_common.params.Map);
   --
   -- Handle the details of the http request.  When a request comes in, the
   -- socket is passed via the start entry point to the task.  The task handles
   -- the processing and closes the socket when finished.  It then waits for
   -- another request.  The end_task entry is used to terminate the task.
   --
   task type request_handler is
      entry start(socket : GNAT.Sockets.Socket_Type);
      entry end_task;
   end request_handler;

   --
   -- Set and get value of debug flag;
   --
   function get_debug return Boolean;
   procedure set_debug(f : Boolean);
private
   --
   --  Build the map for internal procedure calls.  The key strings must match
   --  the identifications in the configuration file.  This needs to be here
   --  so that the various internal routines are accessable.
   --
   procedure build_internal_map;
   --
   -- The number of handler threads to create
   --
   num_handlers : Natural := 10;
   --
   -- Flag to control display of debug messages
   --
   debug : Boolean := False; -- Display requests.
end web_server;
