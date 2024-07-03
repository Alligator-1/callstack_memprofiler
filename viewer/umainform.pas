unit umainform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Generics.Collections,
  laz.VirtualTrees, callstack_memprofiler_common,
  FpDbgLoader, FpDbgDwarf, FpDbgInfo, FpDbgDwarfDataClasses, FpdMemoryTools, DbgIntfBaseTypes,
  ZStream;

type
  TAddrNameDict = specialize TDictionary<Pointer, String>;

  TMainForm = class(TForm)
    btnLoadExecutable: TButton;
    btnLoadMemprofile: TButton;
    vt: TLazVirtualStringTree;
    OpenDialog: TOpenDialog;
    procedure btnLoadExecutableClick(Sender: TObject);
    procedure btnLoadMemprofileClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of string);
    procedure vtBeforeCellPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
    procedure vtCompareNodes(Sender: TBaseVirtualTree; Node1,
      Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
    procedure vtDrawText(Sender: TBaseVirtualTree; TargetCanvas: TCanvas;
      Node: PVirtualNode; Column: TColumnIndex; const CellText: String;
      const CellRect: TRect; var DefaultDraw: Boolean);
    procedure vtExpanded(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vtFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vtGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
  private
    ImageLoaderList: TDbgImageLoaderList;
    MemModel: TFpDbgMemModel;
    DwarfInfo: TFpDwarfInfo;
    MemProfilerData: PByte;
    AddrNameDict: TAddrNameDict;
  public
    procedure LoadMemProfile(filename: string);
    procedure UnLoadMemProfile;
    procedure OpenDWARF(filename: string);
    procedure CloseDWARF;
    function GetDWARFInfoByAddress(addr: Pointer): string;
  end;

const
  s_not_found = '?';

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

procedure TMainForm.btnLoadMemprofileClick(Sender: TObject);
begin
  if OpenDialog.Execute then LoadMemProfile(OpenDialog.FileName);
end;

procedure TMainForm.btnLoadExecutableClick(Sender: TObject);
begin
  if OpenDialog.Execute then OpenDWARF(OpenDialog.FileName);
end;

procedure TMainForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseDWARF;
  UnLoadMemProfile;
  AddrNameDict.Free;
end;

procedure TMainForm.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  vt.NodeDataSize:=SizeOf(PInfo);
  vt.TreeOptions.SelectionOptions:=vt.TreeOptions.SelectionOptions+[toFullRowSelect];
  vt.Header.MinHeight:=50;
  vt.Header.Columns.Add.Text:='addr';              // 0
  vt.Header.Columns.Add.Text:='count';             // 1
  vt.Header.Columns.Add.Text:='mem size';          // 2
  vt.Header.Columns.Add.Text:='real size';         // 3

  vt.Header.Columns.Add.Text:='count alloc';       // 4
  vt.Header.Columns.Add.Text:='mem sum alloc';     // 5
  vt.Header.Columns.Add.Text:='max block alloc';   // 6
  vt.Header.Columns.Add.Text:='min block alloc';   // 7
  vt.Header.Columns.Add.Text:='avg block alloc';   // 8

  vt.Header.Columns.Add.Text:='count free';        // 9
  vt.Header.Columns.Add.Text:='mem sum free';      // 10
  vt.Header.Columns.Add.Text:='max block free';    // 11
  vt.Header.Columns.Add.Text:='min block free';    // 12
  vt.Header.Columns.Add.Text:='avg block free';    // 13
  vt.Header.Columns.Add.Text:='name';              // 14
  for i:=0 to vt.Header.Columns.Count-1 do
  begin
    vt.Header.Columns.Items[i].Options:=vt.Header.Columns.Items[i].Options+[coWrapCaption];
    vt.Header.Columns.Items[i].MinWidth:=50;
    vt.Header.Columns.Items[i].Alignment:=taRightJustify;
    vt.Header.Columns.Items[i].CaptionAlignment:=taLeftJustify;
  end;
  vt.Header.Columns.Items[0].Alignment:=taLeftJustify;
  vt.Header.Columns.Items[14].Alignment:=taLeftJustify;

  vt.Header.Options:=vt.Header.Options+[hoVisible,hoDblClickResize,hoDisableAnimatedResize,hoHeaderClickAutoSort,hoHeightResize];
  vt.DefaultText:='';

  AddrNameDict:=TAddrNameDict.Create;
  DwarfInfo:=nil;
  MemModel:=nil;
end;

procedure TMainForm.FormDropFiles(Sender: TObject; const FileNames: array of string);
begin
  LoadMemProfile(FileNames[0]);
end;

procedure TMainForm.vtBeforeCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
begin
  case Column of
    1..3: begin
            if vsSelected in Node^.States then TargetCanvas.Brush.Color:=$A5FFA5 else TargetCanvas.Brush.Color:=$DCFFDC;
            TargetCanvas.FillRect(CellRect);
          end;
    4..8: begin
            if vsSelected in Node^.States then TargetCanvas.Brush.Color:=$A5A5FF else TargetCanvas.Brush.Color:=$DCDCFF;
            TargetCanvas.FillRect(CellRect);
          end;
    9..13: begin
            if vsSelected in Node^.States then TargetCanvas.Brush.Color:=$FFA5A5 else TargetCanvas.Brush.Color:=$DCDCDC;
            TargetCanvas.FillRect(CellRect);
          end;
  end;
end;

generic function CompareValue<T>(const a, b: T): Integer; inline;
begin
  Result:=1;
  if a=b then Result:=0
  else if a<b then Result:=-1;
end;

procedure TMainForm.vtCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
var
  pnd1, pnd2: PInfo;
begin
  if not Column in [1..13] then Exit;

  pnd1:=PPointer(Sender.GetNodeData(Node1))^;
  pnd2:=PPointer(Sender.GetNodeData(Node2))^;

  case Column of
     1: Result:=specialize CompareValue<Cardinal>(pnd1^.count, pnd2^.count);
     2: Result:=specialize CompareValue<SizeInt>(pnd1^.mem_size, pnd2^.mem_size);
     3: Result:=specialize CompareValue<SizeInt>(pnd1^.real_size, pnd2^.real_size);

     4: Result:=specialize CompareValue<Cardinal>(pnd1^.count_alloc, pnd2^.count_alloc);
     5: Result:=specialize CompareValue<SizeInt>(pnd1^.mem_sum_alloc, pnd2^.mem_sum_alloc);
     6: Result:=specialize CompareValue<Cardinal>(pnd1^.max_block_alloc, pnd2^.max_block_alloc);
     7: Result:=specialize CompareValue<Cardinal>(pnd1^.min_block_alloc, pnd2^.min_block_alloc);
     8: Result:=specialize CompareValue<SizeInt>(pnd1^.avg_alloc_block_size, pnd2^.avg_alloc_block_size);

     9: Result:=specialize CompareValue<Cardinal>(pnd1^.count_free, pnd2^.count_free);
    10: Result:=specialize CompareValue<SizeInt>(pnd1^.mem_sum_free, pnd2^.mem_sum_free);
    11: Result:=specialize CompareValue<Cardinal>(pnd1^.max_block_free, pnd2^.max_block_free);
    12: Result:=specialize CompareValue<Cardinal>(pnd1^.min_block_free, pnd2^.min_block_free);
    13: Result:=specialize CompareValue<SizeInt>(pnd1^.avg_free_block_size, pnd2^.avg_free_block_size);
  end;
end;

procedure TMainForm.vtDrawText(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; const CellText: String; const CellRect: TRect; var DefaultDraw: Boolean);
begin
  TargetCanvas.Font.Color:=clBlack;
end;

procedure TMainForm.vtExpanded(Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  if (Node^.ChildCount=1) and (([vsToggling,vsExpanded] * Node^.FirstChild^.States)=[]) then Sender.ToggleNode(Node^.FirstChild);
  //TLazVirtualStringTree(Sender).Header.AutoFitColumns(False);
end;

procedure TMainForm.vtFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  //Dispose(PNodeData(PPointer(Sender.GetNodeData(Node))^));
end;

procedure TMainForm.vtGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
const
  null_str = '-';
var
  pnd: PInfo;
begin
  pnd:=PPointer(Sender.GetNodeData(Node))^;

  case Column of
     0: CellText:=IntToHex(PtrUInt(pnd^.code_addr)).TrimLeft('0');
     1: CellText:=UIntToStr(pnd^.count);
     2: CellText:=IntToStr(pnd^.mem_size);
     3: CellText:=IntToStr(pnd^.real_size);

     4: CellText:=UIntToStr(pnd^.count_alloc);
     5: if pnd^.count_alloc>0 then CellText:=IntToStr(pnd^.mem_sum_alloc) else CellText:=null_str;
     6: if pnd^.count_alloc>0 then CellText:=UIntToStr(pnd^.max_block_alloc) else CellText:=null_str;
     7: if pnd^.count_alloc>0 then CellText:=UIntToStr(pnd^.min_block_alloc) else CellText:=null_str;
     8: if pnd^.count_alloc>0 then CellText:=IntToStr(pnd^.avg_alloc_block_size) else CellText:=null_str;

     9: CellText:=UIntToStr(pnd^.count_free);
    10: if pnd^.count_free>0 then CellText:='-'+IntToStr(pnd^.mem_sum_free) else CellText:=null_str;
    11: if pnd^.count_free>0 then CellText:='-'+UIntToStr(pnd^.max_block_free) else CellText:=null_str;
    12: if pnd^.count_free>0 then CellText:='-'+UIntToStr(pnd^.min_block_free) else CellText:=null_str;
    13: if pnd^.count_free>0 then CellText:='-'+IntToStr(pnd^.avg_free_block_size) else CellText:=null_str;
    14: CellText:=GetDWARFInfoByAddress(pnd^.code_addr);
    else CellText:='huh?';
  end;
end;

procedure TMainForm.OpenDWARF(filename: string);
begin
  ImageLoaderList := TDbgImageLoaderList.Create(True);
  TDbgImageLoader.Create(filename).AddToLoaderList(ImageLoaderList);

  MemModel := TFpDbgMemModel.Create;
  DwarfInfo := TFpDwarfInfo.Create(ImageLoaderList, nil, MemModel);
  DwarfInfo.LoadCompilationUnits;
end;

procedure TMainForm.CloseDWARF;
begin
  MemModel.Free;
  DwarfInfo.Free;
  DwarfInfo:=nil;
  ImageLoaderList.Free;
end;

function TMainForm.GetDWARFInfoByAddress(addr: Pointer): string;
var
  addr_info: TFpSymbol;
  source, hs: string;
  line: LongWord;
begin
  if AddrNameDict.ContainsKey(addr) then
  begin
    Result:=AddrNameDict.Items[addr];
    Exit;
  end;

  if not Assigned(DwarfInfo) then Exit(s_not_found);

  addr_info:=DwarfInfo.FindProcSymbol(TDBGPtr(addr));
  if Assigned(addr_info) then
  begin
    Result:=addr_info.Name;
    line:=addr_info.Line;
    source:=addr_info.FileName;

    addr_info.Free;

    if source<>'' then
    begin
      if line<>0 then
      begin
        str(line, hs);
        Result:=Result + ' line ' + hs + ' of ' + source;
      end else
      begin
        Result:=Result + ' of ' + source;
      end;
    end;
  end else Result:=s_not_found;

  AddrNameDict.Add(addr, Result);
end;

procedure TMainForm.LoadMemProfile(filename: string);
var
  i,ii: Integer;
  stack_index: integer = -1;
  stack: array of PVirtualNode;
  curr_node: PVirtualNode;

  fs, fds: TStream;
  data_size: LongInt;
  data_pos:  LongInt = 0;
  node_count: LongInt;
  nod: PInfo;

  procedure push(const pvn: PVirtualNode); inline;
  begin
    if stack_index<Length(stack) then SetLength(stack, Length(stack)+10);
    inc(stack_index);
    stack[stack_index]:=pvn;
  end;
  function pop: PVirtualNode; inline;
  begin
    Result:=stack[stack_index];
    dec(stack_index);
  end;

  function get_node_count: LongInt; inline;
  begin
    Result:=PLongInt(@MemProfilerData[data_pos])^;
    inc(data_pos, SizeOf(Result));
  end;
  function get_node: PInfo; inline;
  begin
    Result:=PInfo(@MemProfilerData[data_pos]);
    inc(data_pos, SizeOf(TInfo));
  end;

begin
  fs :=  TFileStream.Create(filename, fmOpenRead);
  fds := TDecompressionStream.Create(fs);
  fs.ReadData(data_size);//, SizeOf(data_size));
  Getmem(MemProfilerData, data_size);

  if data_size=fds.Read(MemProfilerData^, High(LongInt)) then
  begin
    vt.BeginUpdate;

    push(nil);

    while data_pos<data_size do
    begin
      curr_node:=pop;
      node_count:=get_node_count;
      for i:=node_count-1 downto 0 do
      begin
        nod:=get_node;
        push(vt.AddChild(curr_node, nod));

      end;
    end;

    vt.EndUpdate;
    vt.Header.AutoFitColumns(False);
  end
  else
  begin
    UnLoadMemProfile;
    ShowMessage('Something wrong');
  end;

  fds.Free;
  fs.Free;
end;

procedure TMainForm.UnLoadMemProfile;
begin
  Freemem(MemProfilerData);
end;

end.

