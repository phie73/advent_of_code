with Ada.Text_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;

procedure day02_01 is
    Input : Ada.Text_IO.File_Type;
    count: Natural;
begin
    Ada.Text_IO.Open (File => Input, Mode => Ada.Text_IO.In_File, Name => "input02.txt");
    AreaPaper: Natural := 0;
    Ribbon: Natural := 0;
    pattern: constant String := "x";

    AllLines: loop
        exit AllLines when Ada.Text_IO.End_Of_File (Input);
        Line: constant String := Ada.Text_IO.Get_Line (Input);      
        idx: Natural := 0;
        prevIdx: Natural := 0;
        Length: Natural;
        Width: Natural;
        Height: Natural;

        count := Ada.Strings.Fixed.Count(Source => Line, Pattern => pattern);

        for I in 1 .. count loop
            idx := Index (Source => Line, Pattern => pattern, From => idx + 1);
            if idx < 4 then
                Length := Natural'Value(Line(Line'first..idx-1));
                prevIdx := idx;
            else 
                Width := Natural'Value(Line(prevIdx+1..idx-1));
                Height := Natural'Value(Line(idx+1..Line'last));
            end if;
        end loop;

        AreaPaper := AreaPaper + 2*Length*Width + 2*Width*Height + 2*Height*Length;
        Ribbon := Ribbon + Length*Width*Height;
        
        if Length*Width <= Width*Height and Length*Width <= Height*Length then
            AreaPaper := AreaPaper + Length*Width;
            Ribbon := Ribbon + Length + Length + Width + Width;
        elsif Width*Height <= Length*Width and Width*Height <= Height*Length then 
            AreaPaper := AreaPaper + Width*Height;
            Ribbon := Ribbon + Width + Width + Height + Height;
        elsif Height*Length <= Length*Width and Height*Length <= Width*Height then 
            AreaPaper := AreaPaper + Height*Length;
            Ribbon := Ribbon + Height + Height + Length + Length;
        end if;
        
    end loop AllLines;
    Ada.Text_IO.Put_Line("AreaPaper: " & AreaPaper'Image);
    Ada.Text_IO.Put_Line("Ribbon: " & Ribbon'Image);
    Ada.Text_IO.Close (File => Input);   
end day02_01;