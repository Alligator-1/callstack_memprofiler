program example_callstack_memprofiler;

{$mode objfpc}{$H+}

uses
  callstack_memprofiler,
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, umainform
  { you can add units after this };

{$R *.res}

begin
  ReplaceMemoryManager;
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.MainFormOnTaskbar:=True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
  RestoreMemoryManager;
end.

