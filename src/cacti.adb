
with Ada.Text_IO;             use Ada.Text_IO;
with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;
with Interfaces.C;            use Interfaces.C;
with GNAT.Regpat;             use GNAT.Regpat;

with Device;                  use Device;
with Memory.Cache;            use Memory.Cache;
with Memory.SPM;              use Memory.SPM;


package body CACTI is

   subtype file is char;
   type file_ptr is access file;

   function popen(c, t : char_array) return file_ptr;
   pragma Import(C, popen, "popen");

   procedure pclose(s : file_ptr);
   pragma Import(C, pclose, "pclose");

   function fgetc(ptr : file_ptr) return int;
   pragma Import(C, fgetc, "fgetc");

   -- Regular expressions for extracting area and time information.
   area_matcher   : constant Pattern_Matcher
                     := Compile("Data array: Area \(mm2\): ([0-9\.]+)");
   time_matcher   : constant Pattern_Matcher
                     := Compile("Access time \(ns\): ([0-9\.]+)");

   procedure Generate_Cache(cache   : in Cache_Type;
                            file    : in File_Type) is
      wsize    : constant Positive  := Get_Word_Size(cache);
      lsize    : constant Positive  := Get_Line_Size(cache);
      lcount   : constant Positive  := Get_Line_Count(cache);
      bsize    : constant Positive  := wsize * lsize;
      size     : constant Positive  := bsize * lcount;
      assoc    : Natural            := Get_Associativity(cache);
      abits    : constant Positive  := Get_Address_Bits;
      bus_bits : constant Positive  := abits + wsize * 8;
   begin

      -- Size in bytes.
      Put_Line(file, "-size (bytes) " & To_String(size));

      -- Line size in bytes.
      Put_Line(file, "-block size (bytes) " & To_String(bsize));

      -- Associativity (0 for fully-associativity).
      if assoc = lcount then
         assoc := 0;
      end if;
      Put_Line(file, "-associativity " & To_String(assoc));

      -- Ports.
      Put_Line(file, "-read-write port 1");
      Put_Line(file, "-exclusive read port 0");
      Put_Line(file, "-exclusive write port 0");
      Put_Line(file, "-single ended read ports 0");

      -- Banks.
      Put_Line(file, "-UCA bank count 1");

      -- Technology.
      -- TODO support other technologies.
      Put_Line(file, "-technology (u) 0.032");

      -- Cell types.
      Put_Line(file, "-Data array cell type - ""itrs-hp""");
      Put_Line(file, "-Data array peripheral type - ""itrs-hp""");
      Put_Line(file, "-Tag array cell type - ""itrs-hp""");
      Put_Line(file, "-Tag array peripheral type - ""itrs-hp""");

      -- Bus width.
      Put_Line(file, "-output/input bus width " & To_String(bus_bits));

      -- Operating temperature.
      Put_Line(file, "-operating temperature (K) 350");

      -- Type of memory.
      Put_Line(file, "-cache type ""cache""");

      -- Tag size.
      Put_Line(file, "-tag size (b) ""default""");

      -- Access mode.
      Put_Line(file, "-access mode (normal, sequential, fast) - ""normal""");

      -- Cache model.
      Put_Line(file, "-Cache model (NUCA, UCA) - ""UCA""");

      -- Design objective.
      Put_Line(file, "-design objective (weight delay, dynamic power, " &
                     "leakage power, cycle time, area) 0:0:0:0:100");
      Put_Line(file, "-deviate (delay, dynamic power, leakage power, " &
                     "cycle time, area) 60:100000:100000:100000:1000000");

      -- Make sure we get all the information we need.
      Put_Line(file, "-Print level (DETAILED, CONCISE) - ""DETAILED""");

      -- Prefetch width (needed to prevent cacti from crashing).
      Put_Line(file, "-internal prefetch width 8");

   end Generate_Cache;

   procedure Generate_SPM(spm    : in SPM_Type;
                          file   : in File_Type) is
      wsize    : constant Positive  := Get_Word_Size(spm);
      size     : constant Positive  := Get_Size(spm);
      bus_bits : constant Positive  := 8 * wsize;
   begin

      -- Size in bytes.
      Put_Line(file, "-size (bytes) " & To_String(size));

      -- Line size in bytes.
      Put_Line(file, "-block size (bytes) " & To_String(wsize));

      -- Associativity.
      Put_Line(file, "-associativity 1");

      -- Ports.
      Put_Line(file, "-read-write port 1");
      Put_Line(file, "-exclusive read port 0");
      Put_Line(file, "-exclusive write port 0");
      Put_Line(file, "-single ended read ports 0");

      -- Banks.
      Put_Line(file, "-UCA bank count 1");

      -- Technology.
      -- TODO support other technologies.
      Put_Line(file, "-technology (u) 0.032");

      -- Cell types.
      Put_Line(file, "-Data array cell type - ""itrs-hp""");
      Put_Line(file, "-Data array peripheral type - ""itrs-hp""");
      Put_Line(file, "-Tag array cell type - ""itrs-hp""");
      Put_Line(file, "-Tag array peripheral type - ""itrs-hp""");

      -- Bus width.
      Put_Line(file, "-output/input bus width " & To_String(bus_bits));

      -- Operating temperature.
      Put_Line(file, "-operating temperature (K) 350");

      -- Type of memory.
      Put_Line(file, "-cache type ""ram""");

      -- Tag size.
      Put_Line(file, "-tag size (b) ""default""");

      -- Access mode.
      Put_Line(file, "-access mode (normal, sequential, fast) - ""normal""");

      -- Cache model.
      Put_Line(file, "-Cache model (NUCA, UCA) - ""UCA""");

      -- Design objective.
      Put_Line(file, "-design objective (weight delay, dynamic power, " &
                     "leakage power, cycle time, area) 0:0:0:0:100");
      Put_Line(file, "-deviate (delay, dynamic power, leakage power, " &
                     "cycle time, area) 60:100000:100000:100000:1000000");

      -- Make sure we get all the information we need.
      Put_Line(file, "-Print level (DETAILED, CONCISE) - ""DETAILED""");

      -- Prefetch width (needed to prevent cacti from crashing).
      Put_Line(file, "-internal prefetch width 8");

   end Generate_SPM;

   -- Generate the CACTI input.
   procedure Generate(mem  : in Memory_Type'Class;
                      file : in File_Type) is
   begin
      if mem in Cache_Type'Class then
         Generate_Cache(Cache_Type(mem), file);
      elsif mem in SPM_Type'Class then
         Generate_SPM(SPM_Type(mem), file);
      end if;
   end Generate;

   -- Run the CACTI program with parameters from the specified memory.
   procedure Run(mem    : in Memory_Type'Class;
                 result : in out Unbounded_String) is

      ptr         : file_ptr;
      command     : constant String       := "./cacti -infile ";
      cacti_type  : constant char_array   := To_C("r");
      temp        : File_Type;

   begin

      -- Create a temporary file with the parameters.
      Create(File => temp, Name => "cacti-temp.cfg");

      -- Generate the configuration.
      Generate(mem, temp);
      Flush(temp);

      -- Run CACTI.
      declare
         cacti_name : constant char_array := To_C(command & Name(temp));
      begin
         ptr := popen(cacti_name, cacti_type);
      end;
      if ptr /= null then
         loop
            declare
               ch : constant int := fgetc(ptr);
            begin
               exit when ch < 0;
               Append(result, Character'Val(ch));
            end;
         end loop;
         pclose(ptr);
      else
         Put_Line("ERROR: popen failed");
         Delete(temp);
         raise CACTI_Error;
      end if;

      -- Destroy the temporary file.
      Delete(temp);

   end Run;

   function Get_Area(mem : Memory_Type'Class) return Cost_Type is
      buffer   : Unbounded_String;
      result   : Float;
      matches  : Match_Array(0 .. 1);
   begin

      Run(mem, buffer);
      declare
         str : constant String := To_String(buffer);
      begin
         Match(area_matcher, str, matches);
         result := Float'Value(str(matches(1).First .. matches(1).Last));
      exception
         when others =>
            return Cost_Type'Last;
      end;

      -- Here we use units of nm^2, so we need to convert from mm^2.
      return Cost_Type(Float'Ceiling(result * 1000.0 * 1000.0));

   end Get_Area;

   function Get_Time(mem : Memory_Type'Class) return Time_Type is
      buffer   : Unbounded_String;
      result   : Float;
      matches  : Match_Array(0 .. 1);
   begin

      Run(mem, buffer);
      declare
         str : constant String := To_String(buffer);
      begin
         Match(time_matcher, str, matches);
         result := Float'Value(str(matches(1).First .. matches(1).Last));
      end;

      -- Here we assume 1 cycle is 1 ns.
      return Time_Type(Float'Ceiling(result));

   end Get_Time;

end CACTI;
