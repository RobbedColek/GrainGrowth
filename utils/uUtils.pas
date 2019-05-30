unit uUtils;

interface

uses
  Vcl.Graphics;
type
  TUtils = class
    public
      class function IntToBinLowByte(Value: LongWord): string;
      class function DecToBin(N: Integer): string;
      class function GetRandomColor : TColor;
end;

implementation

uses
  Winapi.Windows;
// Function IntToBinLowByte taken from StackOverflow user David Hefferman
// https://stackoverflow.com/questions/21361627/converting-decimal-integer-to-binary-how-and-why-it-works-the-way-it-does
class function TUtils.IntToBinLowByte(Value: LongWord): string;
var
  i: Integer;
begin
  SetLength(Result, 8);
  for i := 1 to 8 do begin
    if ((Value shl (24+i-1)) shr 31) = 0 then begin
      Result[i] := '0'
    end else begin
      Result[i] := '1';
    end;
  end;
end;

// Function DecToBin taken from DelphiExamples
// http://delphiexamples.com/mathematics/dec2bin.html
class function TUtils.DecToBin(N: Integer): string;
var
  S: string;
  i: Integer;
  Negative: Boolean;
begin
  if N<0 then Negative:=True;
  N:=Abs(N);
  for i:=1 to SizeOf(N)*8 do
  begin
    if N<0 then S:=S+'1'
    else S:=S+'0';
    N:=N shl 1;
  end;
  //Delete(S,1,Pos('1',S)-1);
  S := Copy(S, 25, 8);
  if Negative then S:='-'+S;
  Result:=S;
end;

class function TUtils.GetRandomColor : TColor;
begin
  Result := RGB(Random(255), Random(255), Random(255));
end;

end.
