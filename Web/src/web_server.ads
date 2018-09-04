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
   procedure decode_internal(s : GNAT.Sockets.Stream_Access; name : String;
                             p : web_common.params.Map);
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
   -- Flag to indicate that the configuration file has changed and needs to be
   -- reloaded.  This would typically be used during development or debugging.
   --
   reload_configuration : Boolean := False;

   --
   -- Set and get value of debug flag;
   --
   function get_debug return Boolean;
   procedure set_debug(f : Boolean);
private
   --
   -- The number of handler threads to create
   --
   num_handlers : Natural := 10;
   --
   -- Flag to control display of debug messages
   --
   debug : Boolean := False; -- Display requests.
end web_server;
