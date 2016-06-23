program ThreadExample;

{$R 'teefiles.res' 'teefiles.rc'}

uses
  Forms,
  Main_Form in 'Main_Form.pas' {MainForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
