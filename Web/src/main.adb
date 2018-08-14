with web_server;
with rs485;
with Ada.Text_IO;

procedure Main is

begin
   rs485.state_machine.start;
   web_server.server;
end Main;
