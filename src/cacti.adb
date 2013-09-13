
with Ada.Text_IO;                   use Ada.Text_IO;
with Ada.Strings.Unbounded;         use Ada.Strings.Unbounded;
with Ada.Containers.Ordered_Maps;   use Ada.Containers;
with Interfaces.C;                  use Interfaces.C;
with GNAT.Regpat;                   use GNAT.Regpat;

with Device;                        use Device;
with Memory.Cache;                  use Memory.Cache;
with Memory.SPM;                    use Memory.SPM;


package body CACTI is

   subtype file is char;
   type file_ptr is access file;

   type Parameter_Type is record
      size           : Positive;
      block_size     : Positive;
      bus_bits       : Positive;
      associativity  : Natural := 1;
      is_cache       : Boolean := False;
   end record;

   type Result_Type is record
      area           : Cost_Type;
      time           : Time_Type;
   end record;

   function "<"(a, b : Parameter_Type) return Boolean is
   begin
      if a.size /= b.size then
         return a.size < b.size;
      end if;
      if a.block_size /= b.block_size then
         return a.block_size < b.block_size;
      end if;
      if a.bus_bits /= b.bus_bits then
         return a.bus_bits < b.bus_bits;
      end if;
      if a.associativity /= b.associativity then
         return a.associativity < b.associativity;
      end if;
      if a.is_cache /= b.is_cache then
         return a.is_cache;
      end if;
      return False;
   end "<";

   package Result_Maps is new Ordered_Maps(Parameter_Type, Result_Type);

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

   -- Cache of results.
   results : Result_Maps.Map;

   -- Generate CACTI input.
   procedure Generate(file    : in File_Type;
                      param   : in Parameter_Type) is
   begin

      -- Size in bytes.
      Put_Line(file, "-size (bytes) " & To_String(param.size));

      -- Line size in bytes.
      Put_Line(file, "-block size (bytes) " & To_String(param.block_size));

      -- Associativity (0 for fully-associativity).
      Put_Line(file, "-associativity " & To_String(param.associativity));

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
      Put_Line(file, "-output/input bus width " & To_String(param.bus_bits));

      -- Operating temperature.
      Put_Line(file, "-operating temperature (K) 350");

      -- Type of memory.
      if param.is_cache then
         Put_Line(file, "-cache type ""cache""");
      else
         Put_Line(file, "-cache type ""ram""");
      end if;

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

   end Generate;

   -- Get CACTI parameters for a cache.
   function Get_Cache(cache : Cache_Type) return Parameter_Type is
      wsize    : constant Positive  := Get_Word_Size(cache);
      lsize    : constant Positive  := Get_Line_Size(cache);
      lcount   : constant Positive  := Get_Line_Count(cache);
      bsize    : constant Positive  := wsize * lsize;
      size     : constant Positive  := bsize * lcount;
      assoc    : constant Natural   := Get_Associativity(cache);
      abits    : constant Positive  := Get_Address_Bits;
      bus_bits : constant Positive  := abits + wsize * 8;
      param    : Parameter_Type;
   begin
      param.size        := size;
      param.block_size  := bsize;
      param.bus_bits    := bus_bits;
      param.is_cache    := True;
      if assoc = lcount then
         param.associativity := 0;
      else
         param.associativity := assoc;
      end if;
      return param;
   end Get_Cache;

   -- Get CACTI parameters for an SPM.
   function Get_SPM(spm : SPM_Type) return Parameter_Type is
      wsize    : constant Positive  := Get_Word_Size(spm);
      size     : constant Positive  := Get_Size(spm);
      bus_bits : constant Positive  := 8 * wsize;
      param    : Parameter_Type;
   begin
      param.size        := size;
      param.block_size  := wsize;
      param.bus_bits    := bus_bits;
      return param;
   end Get_SPM;

   -- Get CACTI parameters.
   function Get_Parameter(mem : Memory_Type'Class) return Parameter_Type is
   begin
      if mem in Cache_Type'Class then
         return Get_Cache(Cache_Type(mem));
      elsif mem in SPM_Type'Class then
         return Get_SPM(SPM_Type(mem));
      else
         raise CACTI_Error;
      end if;
   end Get_Parameter;

   -- Run the CACTI program with parameters from the specified memory.
   function Run(mem : Memory_Type'Class) return Result_Type is

      param       : constant Parameter_Type  := Get_Parameter(mem);
      command     : constant String          := "./cacti -infile ";
      cacti_type  : constant char_array      := To_C("r");
      cursor      : Result_Maps.Cursor;
      buffer      : Unbounded_String;
      ptr         : file_ptr;
      temp        : File_Type;
      result      : Result_Type;
      matches     : Match_Array(0 .. 1);

   begin

      -- Check if we've already run CACTI with these parameters.
      cursor := results.Find(param);
      if Result_Maps."/="(cursor, Result_Maps.No_Element) then
         return Result_Maps.Element(cursor);
      end if;

      -- Create a temporary file with the parameters.
      Create(File => temp);
      Generate(temp, param);
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
               Append(buffer, Character'Val(ch));
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

      -- Extract the area and time from the CACTI results.
      declare
         str   : constant String := To_String(buffer);
         value : Float;
      begin

         -- Here we use units of nm^2, so we need to convert from mm^2.
         Match(area_matcher, str, matches);
         value := Float'Value(str(matches(1).First .. matches(1).Last));
         result.area := Cost_Type(Float'Ceiling(value * 1000.0 * 1000.0));

         -- Here we assume 1 cycle is 1 ns.
         Match(time_matcher, str, matches);
         value := Float'Value(str(matches(1).First .. matches(1).Last));
         result.time := Time_Type(Float'Ceiling(value));

      exception
         when others =>
            result.area := Cost_Type'Last;
            result.time := Time_Type'Last;
      end;

      -- Insert the result to our results map.
      results.Insert(param, result);

      return result;

   end Run;

   function Get_Area(mem : Memory_Type'Class) return Cost_Type is
      result : constant Result_Type := Run(mem);
   begin
      return result.area;
   end Get_Area;

   function Get_Time(mem : Memory_Type'Class) return Time_Type is
      result : constant Result_Type := Run(mem);
   begin
      return result.time;
   end Get_Time;

end CACTI;
