with Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

procedure day01_01 is
    Input : Ada.Text_IO.File_Type;
begin
    Ada.Text_IO.Open (File => Input, Mode => Ada.Text_IO.In_File, Name => "input01.txt");
    Floor: Integer := 0;

    AllLines: loop
        exit AllLines when Ada.Text_IO.End_Of_File (Input);
        Line: constant Unbounded_String := To_Unbounded_String (Ada.Text_IO.Get_Line (Input));
        for I in 1 .. Length (Line) loop
            if Element (Line, I) = '(' then
                Floor := Floor + 1;
            end if;
            if Element (Line, I) = ')' then 
                Floor := Floor - 1;
            end if;
            if Floor = -1 then -- part 2
                Ada.Text_IO.Put_Line(Item => I'Image);
                exit;
            end if;
        end loop;
    end loop AllLines;
    Ada.Text_IO.Put_Line (Item => Floor'Image);
    Ada.Text_IO.Close (File => Input);   
end day01_01;