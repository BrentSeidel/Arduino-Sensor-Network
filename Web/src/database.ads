with Ada.Text_IO;
with common;
with BBS.web_common;
--
--  This package provides a database interface to the RS-485 bus monitor.  Data from
--  the bus can be saved for later analysis.
--
package database is
   --
   --  The different kinds of databases supported.
   --    None - no database selected so no logging is done
   --    CSV - Data is written to CSV files with separate files for each type of data
   --    SQLite - Data is written to an SQLite database
   --
   type database_type is (None, CSV, SQLite);

   --
   --  Initialize the database interface by selecting which type of database to use.
   --
   procedure init(kind : database_type);
   --
   --  Log the data record to the database.
   --
   procedure log(node : Integer; d : common.data_record);
   --
   --  Cleanly close any log files
   --
   procedure end_log;
   --
   --  Flags to indicate which data should be logged
   --
   enable_info    : BBS.web_common.protected_flag;
   enable_BME280  : BBS.web_common.protected_flag;
   enable_CCS811  : BBS.web_common.protected_flag;
   enable_TSL2561 : BBS.web_common.protected_flag;
   --
   --  What kind of database is selected
   --
   selected_db : database_type := None;
private
   --
   --  Path where log files are stored
   --
   log_path : String := "./";
   --
   --  Files for CSV database
   --
   Info_file   : Ada.Text_IO.File_Type;
   BME280_file : Ada.Text_IO.File_Type;
   CCS811_file : Ada.Text_IO.File_Type;
   TSL2561_file : Ada.Text_IO.File_Type;
   --
   --  Data and database specific data logging procedures.
   --
   procedure log_csv(node : Integer; d : common.data_record);
   procedure log_sqlite(node : Integer; d : common.data_record);
end database;
