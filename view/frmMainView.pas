unit frmMainView;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, System.Actions, Vcl.ActnList,
  Vcl.StdCtrls, Vcl.Buttons, Vcl.ExtCtrls, cCellularAutomata, Vcl.ComCtrls;

type
  TForm1 = class(TForm)
    Image: TImage;
    MainMenu: TPanel;
    ActionList: TActionList;
    ActDraw: TAction;
    ActClear: TAction;
    LblHeight: TLabel;
    EdtIterationsGameOfLife: TEdit;
    EdtWidthGameOfLife: TEdit;
    LblWidth: TLabel;
    BtnClear: TSpeedButton;
    TimerGrainGrowth: TTimer;
    BtnStart: TSpeedButton;
    EdtInterval: TEdit;
    LblInterval: TLabel;
    ActClearButton: TAction;
    LblBoundaryConditions: TLabel;
    CmbBoundaryConditions: TComboBox;
    LblNucleation: TLabel;
    CmbNucleation: TComboBox;
    EdtNucleationVariable1: TEdit;
    EdtNucleationVariable2: TEdit;
    LblNucleationVariable1: TLabel;
    LblNucleationVariable2: TLabel;
    ActGrainGrowthTimer: TAction;
    CmbNeighbourhoodType: TComboBox;
    LblNeighbourhoodType: TLabel;
    procedure ActClearExecute(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure DrawGrid;
    procedure Draw;
    procedure ImageMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure TimerGrainGrowthTimer(Sender: TObject);
    procedure ActClearButtonExecute(Sender: TObject);
    procedure CmbNucleationChange(Sender: TObject);
    procedure ActGrainGrowthTimerExecute(Sender: TObject);
  private
    FCellularAutomata : TCellularAutomata;

    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.ActClearButtonExecute(Sender: TObject);
begin
  ActClear.Execute;

  FCellularAutomata.Free;
  FCellularAutomata := TCellularAutomata.Create;
  FCellularAutomata.PrepareGridGameOfLife(StrToInt(EdtIterationsGameOfLife.Text), StrToInt(EdtWidthGameOfLife.Text));
end;

procedure TForm1.ActClearExecute(Sender: TObject);
begin
  Image.Canvas.Brush.Color := clWhite;
  Image.Canvas.Pen.Color := clBlack;
  Image.Canvas.FillRect(Rect(0, 0, Image.Width, Image.Height));

  DrawGrid;
end;

procedure TForm1.Draw;
var xScale, yScale, x, y, I, J, Width, Iterations, Scale, CounterTmp: Integer;
begin
  if Assigned(FCellularAutomata) then begin

    CounterTmp := 0;

    Image.Canvas.Pen.Color := clBlack;
    Image.Canvas.Brush.Color := clBlack;

    Width := StrToInt(EdtWidthGameOfLife.Text);
    Iterations := StrToInt(EdtIterationsGameOfLife.Text);

    xScale := Image.Width div Width;
    yScale := Image.Height div StrToInt(EdtIterationsGameOfLife.Text);

    if xScale > yScale then Scale := yScale else Scale := xScale;

    y := 0;
    for I := 0 to Iterations - 1 do begin
      x := 0;
      for J := 0 to Width - 1 do begin
        if FCellularAutomata.GetValue(I, J) <> 0 then begin
          Image.Canvas.Pen.Color := FCellularAutomata.GetColor(FCellularAutomata.GetValue(I, J));
          Image.Canvas.Brush.Color := FCellularAutomata.GetColor(FCellularAutomata.GetValue(I, J));
          Image.Canvas.Rectangle(x, y, x + Scale, y + Scale);
        end else CounterTmp := CounterTmp + 1;
        x := x + Scale;
      end;
      y := y + Scale;
    end;

    if CounterTmp = 0 then ActGrainGrowthTimer.Execute;

  end;
end;

procedure TForm1.DrawGrid;
var xScale, yScale, I, Width, Iterations, Scale: Integer;
begin
  Image.Canvas.Pen.Color := clBlack;

  Width := StrToInt(EdtWidthGameOfLife.Text);
  Iterations := StrToInt(EdtIterationsGameOfLife.Text);

  xScale := Image.Width div Width;
  yScale := Image.Height div Iterations;

  if xScale > yScale then Scale := yScale else Scale := xScale;

  for I := 0 to Width do begin
    Image.Canvas.MoveTo(Scale * I, 0);
    Image.Canvas.LineTo(Scale * I, Iterations * 4 * (Scale div 4));
  end;

  for I := 0 to Iterations do begin
    Image.Canvas.MoveTo(0, Scale * I);
    Image.Canvas.LineTo(Width * 4 * (Scale div 4), Scale * I);
  end;

end;

procedure TForm1.FormShow(Sender: TObject);
begin
  Randomize();

  Image.Canvas.Brush.Color := clWhite;
  Image.Canvas.FillRect(Rect(0, 0, Image.Width, Image.Height));

  DrawGrid;

  FCellularAutomata := TCellularAutomata.Create;
  FCellularAutomata.PrepareGridGameOfLife(StrToInt(EdtIterationsGameOfLife.Text), StrToInt(EdtWidthGameOfLife.Text));
end;

procedure TForm1.ImageMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var xScale, yScale, Scale: Integer;
begin
  if Assigned(FCellularAutomata) then begin
    xScale := Image.Width div StrToInt(EdtWidthGameOfLife.Text);
    yScale := Image.Height div StrToInt(EdtIterationsGameOfLife.Text);

    if xScale > yScale then Scale := yScale else Scale := xScale;

    if (x > FCellularAutomata.Width * Scale) then Exit;
    if (y > FCellularAutomata.Iterations * Scale) then Exit;

    if FCellularAutomata.GetValue(y div Scale, x div Scale) <> 0 then Exit;

    FCellularAutomata.Counter := FCellularAutomata.Counter + 1;
    FCellularAutomata.SetValue(y div Scale, x div Scale, FCellularAutomata.Counter);
    FCellularAutomata.SetColor;

    ActClear.Execute;
    Draw;
  end;

end;

procedure TForm1.TimerGrainGrowthTimer(Sender: TObject);
begin
  Try
    FCellularAutomata.StepGrainGrowth;

    actClear.Execute;
    Draw;
  Except
    on E : Exception do
    begin
      ActGrainGrowthTimer.Execute;
      ShowMessage('Exception class name = '+E.ClassName);
      ShowMessage('Exception message = '+E.Message);
    end;
  End;
end;

procedure TForm1.ActGrainGrowthTimerExecute(Sender: TObject);
var Variable1, Variable2 : Integer;
begin
  timerGrainGrowth.Interval := StrToInt(EdtInterval.Text);
  timerGrainGrowth.Enabled := not timerGrainGrowth.Enabled;

  if timerGrainGrowth.Enabled then begin
    ActGrainGrowthTimer.Caption := 'Stop';
    EdtInterval.Enabled := False;
    BtnClear.Enabled := False;

    Variable1 := 0;
    Variable2 := 0;

    if EdtNucleationVariable1.Text <> '' then Variable1 := StrToInt(EdtNucleationVariable1.Text);
    if EdtNucleationVariable2.Text <> '' then Variable2 := StrToInt(EdtNucleationVariable2.Text);

    FCellularAutomata.SetNucleation(CmbNucleation.ItemIndex, Variable1, Variable2);
    FCellularAutomata.NeighbourhoodType := CmbNeighbourhoodType.ItemIndex;
    FCellularAutomata.BoundaryCondition := CmbBoundaryConditions.ItemIndex;

  end else begin
    ActGrainGrowthTimer.Caption := 'Start';
    EdtInterval.Enabled := True;
    BtnClear.Enabled := True;
  end;
end;

procedure TForm1.CmbNucleationChange(Sender: TObject);
begin

  if CmbNucleation.ItemIndex = 0 then begin
    LblNucleationVariable1.Caption := 'Number in rows';
    LblNucleationVariable2.Caption := 'Number in columns';
    LblNucleationVariable1.Visible := True;
    LblNucleationVariable2.Visible := True;
    EdtNucleationVariable1.Visible := True;
    EdtNucleationVariable2.Visible := True;
  end;

  if CmbNucleation.ItemIndex = 1 then begin
    LblNucleationVariable1.Caption := 'Radius';
    LblNucleationVariable2.Caption := 'Number of grains';
    LblNucleationVariable1.Visible := True;
    LblNucleationVariable2.Visible := True;
    EdtNucleationVariable1.Visible := True;
    EdtNucleationVariable2.Visible := True;
  end;

  if CmbNucleation.ItemIndex = 2 then begin
    LblNucleationVariable1.Caption := 'Number of grains';
    LblNucleationVariable2.Caption := '';
    LblNucleationVariable1.Visible := True;
    LblNucleationVariable2.Visible := False;
    EdtNucleationVariable1.Visible := True;
    EdtNucleationVariable2.Visible := False;
  end;

  if CmbNucleation.ItemIndex = 3 then begin
    LblNucleationVariable1.Visible := False;
    LblNucleationVariable2.Visible := False;
    EdtNucleationVariable1.Visible := False;
    EdtNucleationVariable2.Visible := False;
  end;
end;

end.
