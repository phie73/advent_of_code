with Ada.Text_IO; use Ada.Text_IO;

procedure day01_2 is
  Input : File_Type;
  Sum : Natural := 0;
  Filename : constant String := "input01";

  procedure GetDigits ( Text : String; FirstDigit, LastDigit : out Natural ) is
    IsFirst : Boolean := True;
    Index : Natural := 1;

    procedure SetDigit ( N : Natural ) is
    begin
      if IsFirst then
        FirstDigit := N;
        LastDigit := FirstDigit;
        IsFirst := False;
      else
        LastDigit := N;
      end if;
    end SetDigit;

    procedure ParseDigit ( Text : String ) is
    begin
      case Text is
        when "one" => SetDigit (1);
        when "two" => SetDigit (2);
        when "three" => SetDigit (3);
        when "four" => SetDigit (4);
        when "five" => SetDigit (5);
        when "six" => SetDigit (6);
        when "seven" => SetDigit (7);
        when "eight" => SetDigit (8);
        when "nine" => SetDigit (9);
        when others => null;
      end case;
    end ParseDigit;

  begin
    for C of Text loop
      if C in '1' .. '9' then
        SetDigit ( Natural'Value (C & "") );
      elsif C in 'o' | 't' | 'f' | 's' | 'e' | 'n' then
        for L in 2 .. 4 loop
          if Index <= Text'Length - L then
            ParseDigit ( Text ( Index .. Index + L ) );
          end if;
        end loop;
      end if;
      Index := @ + 1;
    end loop;
  end GetDigits;

  begin
    Input.Open (In_File, Filename);
    loop
      declare
        Line : constant String := Input.Get_Line;
        FirstDigit, LastDigit : Natural;
      begin
        GetDigits (Line, FirstDigit, LastDigit);
        Sum := @ + (FirstDigit * 10 + LastDigit);
      end;
      exit when Input.End_Of_File;
    end loop;
    Input.Close;
    Put_Line("hmm: " & Sum'Image);
end day01_2;
