unit nodetreedataio;
{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, nodetree, ZStream;

type
  TDefaultNodeDataIO = class sealed(TAbstractDataIO)
    fs, cs, ds: TStream;
    constructor Create(filename: String);
    destructor Destroy; override;
    procedure ReadData(var data; size: UInt64); reintroduce; inline;
    procedure WriteData(const data; size: UInt64); reintroduce; inline;
  end;

implementation

constructor TDefaultNodeDataIO.Create(filename: String);
begin
  inherited Create;

  if filename='' then filename:=FormatDateTime('yyyymmddHHMMSS', Now)+'.memprof';
  fs := TFileStream.Create(filename, specialize IfThen<Word>(FileExists(filename), fmOpenReadWrite, fmCreate));
  cs := TCompressionStream.Create(clfastest, fs);
  ds := TDecompressionStream.create(fs);
end;

destructor TDefaultNodeDataIO.Destroy;
begin
  ds.Free;
  cs.Free;
  fs.Free;

  inherited Destroy;
end;

procedure TDefaultNodeDataIO.ReadData(var data; size: UInt64);
begin
  ds.Read(data, size);
end;

procedure TDefaultNodeDataIO.WriteData(const data; size: UInt64);
begin
  cs.Write(data, size);
end;

end.

