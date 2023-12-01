with Ada.Text_IO;
with Ada.Strings.Maps;
with Ada.Strings.Fixed;

procedure day01_1 is
  Input : Ada.Text_IO.File_Type;
  Digit : constant Ada.Strings.Maps.Character_Set := Ada.Strings.Maps.To_Set (Span => (Low => '0', High => '9') );
  Sum : Natural := 0; 
begin
  Ada.Text_IO.Open (File => Input, Mode => Ada.Text_IO.In_File, Name => "input01");
  AllLines: loop
    exit AllLines when Ada.Text_IO.End_Of_File (Input);
    InLine : declare
      Line: constant String := Ada.Text_IO.Get_Line (Input);
      FirstDigit : constant Natural := Ada.Strings.Fixed.Index (Line, Digit);
      LastDigit : constant Natural := Ada.Strings.Fixed.Index (Line, Digit, Going => Ada.Strings.Backward);
    begin -- InLine
      if FirstDigit = 0 or LastDigit = 0 then
        Ada.Text_IO.Put_Line (Item => "line wo 2 numbers: " & Line);
      else
        Sum := Sum + Integer'Value (Line (FirstDigit) & Line (LastDigit) );
      end if;
    end InLine;
  end loop AllLines;
  Ada.Text_IO.Close (File => Input);
  Ada.Text_IO.Put_Line (Item => Sum'Image);
end day01_1;
