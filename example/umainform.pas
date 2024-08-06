unit umainform;
{$mode objfpc}{$H+}

interface

uses
  Forms, Dialogs, StdCtrls, callstack_memprofiler;

type
  TMainForm = class(TForm)
    btnReset: TButton;
    btnLoad: TButton;
    btnSave: TButton;
    OpenDialog: TOpenDialog;
    procedure btnResetClick(Sender: TObject);
    procedure btnLoadClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

procedure TMainForm.btnResetClick(Sender: TObject);
begin
  ResetData;
end;

procedure TMainForm.btnLoadClick(Sender: TObject);
begin
  if OpenDialog.Execute then
  begin
    LoadProfileFromFile(OpenDialog.FileName);
  end;
end;

procedure TMainForm.btnSaveClick(Sender: TObject);
begin
  SaveProfileToFile;
end;

end.

