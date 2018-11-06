with GNAT.Sockets;
with bbs.web_common;
with rs485;
use type rs485.message_types;
--
--  This package contains the various internal routines to generate pages, data.
--  figures, and the like.  This is where the main customization will occure.
--
package internal is
   --
   --  Return the count of transactions as an xml message
   --
   procedure xml_count(s : GNAT.Sockets.Stream_Access;
                       h : bbs.web_common.params.Map;
                       p : bbs.web_common.params.Map);
   --
   --  Display the configuration data as a table.
   --
--   procedure html_show_config(s : GNAT.Sockets.Stream_Access;
--                              h : bbs.web_common.params.Map;
--                              p : bbs.web_common.params.Map);
   --
   --  Display table consisting of address 0 (device ID) for all devices
   --
   procedure html_devices(s : GNAT.Sockets.Stream_Access;
                          h : bbs.web_common.params.Map;
                          p : bbs.web_common.params.Map);
   --
   --  Provide data in XML format
   --
   --  Send length of data store
   --
   procedure xml_devices(s : GNAT.Sockets.Stream_Access;
                         h : bbs.web_common.params.Map;
                         p : bbs.web_common.params.Map);
   --
   --  Get device name
   --
   procedure xml_device_name(s : GNAT.Sockets.Stream_Access;
                             h : bbs.web_common.params.Map;
                             p : bbs.web_common.params.Map);
   --
   --  Get device data - all data from a device
   --
   procedure xml_device_data(s : GNAT.Sockets.Stream_Access;
                             h : bbs.web_common.params.Map;
                             p : bbs.web_common.params.Map);
   --
   --  Request to send a command to the controller.
   --
   procedure xml_send_command(s : GNAT.Sockets.Stream_Access;
                              h : bbs.web_common.params.Map;
                              p : bbs.web_common.params.Map);
   --
   --  Set and retrieve debugging flags
   --
   procedure xml_debugging(s : GNAT.Sockets.Stream_Access;
                           h : bbs.web_common.params.Map;
                           p : bbs.web_common.params.Map);

private
   CRLF : String renames bbs.web_common.CRLF;
   --
   --  Some helper functions - to be used only by other internal functions.
   --  They won't work if called with the wrong variant of rs485.data_record.
   --
   --
   --  Display an info record as a table that can be nested in another table
   --
   procedure html_info(s : GNAT.Sockets.Stream_Access; d : rs485.data_record);
   --
   --  Display an BME280 record as a table that can be nested in another table.
   --  The metric boolean controls whether data is displayed in metric or
   --  standard.
   --
   procedure html_bme280(s : GNAT.Sockets.Stream_Access; d : rs485.data_record;
                        metric : Boolean);
   --
   --  Display an discrete record as a table that can be nested in another table
   --
   procedure html_discrete(s : GNAT.Sockets.Stream_Access; d : rs485.data_record);
   --
   --  Display an CCS811 record as a table that can be nested in another table
   --
   procedure html_ccs811(s : GNAT.Sockets.Stream_Access; d : rs485.data_record);
   --
   --  Display an TSL2561 record as a table that can be nested in another table
   --
   procedure html_tsl2561(s : GNAT.Sockets.Stream_Access; d : rs485.data_record);
   --
   --  Provide data in XML format
   --
   --  Provide an XML version of the info message
   --
   procedure xml_info_msg(s : GNAT.Sockets.Stream_Access; d : rs485.data_record);
   --
   --  Provide an XML version of the BME280 message
   --
   procedure xml_BME280_msg(s : GNAT.Sockets.Stream_Access; d : rs485.data_record);
   --
   --  Provide an XML version of the discrete message
   --
   procedure xml_discrete_msg(s : GNAT.Sockets.Stream_Access; d : rs485.data_record);
   --
   --  Provide an XML version of the analog message
   --
   procedure xml_analog_msg(s : GNAT.Sockets.Stream_Access; d : rs485.data_record);
   --
   --  Provide an XML version of the CCS811 message
   --
   procedure xml_ccs811_msg(s : GNAT.Sockets.Stream_Access; d : rs485.data_record);
   --
   --  Provide an XML version of the TSL2561 message
   --
   procedure xml_tsl2561_msg(s : GNAT.Sockets.Stream_Access; d : rs485.data_record);
   --
   --  Provide an XML version of the PCA9685 message
   --
   procedure xml_pca9685_msg(s : GNAT.Sockets.Stream_Access; d : rs485.data_record);
end;

