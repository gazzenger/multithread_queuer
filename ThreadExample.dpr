program ThreadExample;

{$R 'teefiles.res' 'teefiles.rc'}

uses
  Forms,
  Main_Form in 'Main_Form.pas' {MainForm},
  about_form in 'about_form.pas' {about_window};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(Tabout_window, about_window);
  Application.Run;
end.
