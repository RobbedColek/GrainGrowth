program GrainGrowth;

uses
  //FastMM4,
  Vcl.Forms,
  frmMainView in 'view\frmMainView.pas' {Form1},
  cCellularAutomata in 'controller\cCellularAutomata.pas',
  uUtils in 'utils\uUtils.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
