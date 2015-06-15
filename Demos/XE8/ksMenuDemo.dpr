program ksMenuDemo;

uses
  System.StartUpCopy,
  FMX.Forms,
  untMain in 'untMain.pas' {Form6};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm6, Form6);
  Application.Run;
end.
