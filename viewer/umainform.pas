unit umainform;
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Generics.Collections,
  laz.VirtualTrees, callstack_memprofiler_common,
  FpDbgLoader, FpDbgDwarf, FpDbgInfo, FpDbgDwarfDataClasses, FpdMemoryTools, DbgIntfBaseTypes,
  ZStream
  ;

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
    procedure vtExpanding(Sender: TBaseVirtualTree; Node: PVirtualNode;
      var Allowed: Boolean);
    procedure vtGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
  private
    ImageLoaderList: TDbgImageLoaderList;
{$IF DECLARED(TFpDbgMemModel)}
    MemModel: TFpDbgMemModel;
{$ENDIF}
    DwarfInfo: TFpDwarfInfo;
    AddrNameDict: TAddrNameDict;
    MemProfilerData: PByte;
    node_index_arr: PUInt64;
    node_index_arr_size: LongInt;
    data_pos:  UInt64;
  public
    procedure LoadNode(const node: PVirtualNode);
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
  vt.NodeDataSize:=SizeOf(PInfo)    // data
                  +SizeOf(Pointer)  // index (пусть будет Pointer пока-что)
                  +SizeOf(Pointer)  // флаг loaded (пусть будет Pointer пока-что)
                  ;
  vt.TreeOptions.SelectionOptions:=vt.TreeOptions.SelectionOptions+[toFullRowSelect];
  vt.Header.MinHeight:=50;
  with vt.Header.Columns do
  begin
    Add.Text:='addr';              // 0
    Add.Text:='count';             // 1
    Add.Text:='mem size';          // 2
    Add.Text:='real size';         // 3

    Add.Text:='count alloc';       // 4
    Add.Text:='mem sum alloc';     // 5
    Add.Text:='max block alloc';   // 6
    Add.Text:='min block alloc';   // 7
    Add.Text:='avg block alloc';   // 8

    Add.Text:='count free';        // 9
    Add.Text:='mem sum free';      // 10
    Add.Text:='max block free';    // 11
    Add.Text:='min block free';    // 12
    Add.Text:='avg block free';    // 13
    Add.Text:='name';              // 14
  end;
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
{$IF DECLARED(TFpDbgMemModel)}
  MemModel:=nil;
{$ENDIF}
  MemProfilerData:=nil;
  node_index_arr:=nil;
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

function get_group_index: LongInt; inline;
begin
  Result:=PLongInt(@MainForm.MemProfilerData[MainForm.data_pos])^;
  inc(MainForm.data_pos, SizeOf(Result));
end;
function get_node_count: LongInt; inline;
begin
  Result:=PLongInt(@MainForm.MemProfilerData[MainForm.data_pos])^;
  inc(MainForm.data_pos, SizeOf(Result));
end;
function get_node_has_children: Boolean; inline;
begin
  Result:=PBoolean(@MainForm.MemProfilerData[MainForm.data_pos])^;
  inc(MainForm.data_pos, SizeOf(Result));
end;
function get_node: PInfo; inline;
begin
  Result:=PInfo(@MainForm.MemProfilerData[MainForm.data_pos]);
  inc(MainForm.data_pos, SizeOf(TInfo));
end;

procedure TMainForm.vtExpanded(Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  if (Node^.ChildCount=1) and (([vsToggling,vsExpanded] * Node^.FirstChild^.States)=[]) then Sender.ToggleNode(Node^.FirstChild);

  //TLazVirtualStringTree(Sender).Header.AutoFitColumns(False); // need timeout
end;

procedure TMainForm.vtExpanding(Sender: TBaseVirtualTree; Node: PVirtualNode; var Allowed: Boolean);
  function GetChildCount: LongInt;
  var
    group_index: LongInt;
  begin
    group_index:=LongInt(PPointer(vt.GetNodeData(Node))[1]);
    data_pos:=node_index_arr[group_index];

    //Assert(group_index=get_group_index);
    get_group_index;

    Result := get_node_count;
  end;
begin
  if (Node^.FirstChild=nil) and (GetChildCount<>0) then LoadNode(Node);
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

procedure TMainForm.LoadNode(const node: PVirtualNode);
var
  i: Integer;
  new_node: PVirtualNode;
  group_index: LongInt;
begin
  if (PPointer(vt.GetNodeData(node))[2]=nil) then
  begin
    vt.BeginUpdate;

    group_index:=LongInt(PPointer(vt.GetNodeData(node))[1]);
    data_pos:=node_index_arr[group_index];

    //Assert(group_index=get_group_index);
    get_group_index;

    for i:=get_node_count-1 downto 0 do
    begin
      new_node:=vt.AddChild(node, get_node);
      vt.HasChildren[new_node]:=get_node_has_children;

      PPointer(vt.GetNodeData(new_node))[1]:=Pointer(get_group_index);
      PPointer(vt.GetNodeData(new_node))[2]:=nil; // на самом деле это булевый флаг 1 - ноды созданы, 0 - не созданы
    end;

    PPointer(vt.GetNodeData(node))[2]:=Pointer(1);

    vt.EndUpdate;
  end;
end;

procedure TMainForm.OpenDWARF(filename: string);
begin
  CloseDWARF;

  ImageLoaderList := TDbgImageLoaderList.Create(True);
  TDbgImageLoader.Create(filename).AddToLoaderList(ImageLoaderList);

{$IF DECLARED(TFpDbgMemModel)}
  MemModel := TFpDbgMemModel.Create;
  DwarfInfo := TFpDwarfInfo.Create(ImageLoaderList, nil, MemModel);
{$ELSE}
  DwarfInfo := TFpDwarfInfo.Create(ImageLoaderList, nil);
{$ENDIF}


  DwarfInfo.LoadCompilationUnits;
end;

procedure TMainForm.CloseDWARF;
begin
{$IF DECLARED(TFpDbgMemModel)}
  MemModel.Free;
  MemModel:=nil;
{$ENDIF}
  DwarfInfo.Free;
  DwarfInfo:=nil;
  ImageLoaderList.Free;
  ImageLoaderList:=nil;
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
  i: Integer;
  node: PVirtualNode;

  fs, ds: TStream;
  data_size: LongInt;
begin
  AddrNameDict.Clear;
  vt.Clear;

  fs :=  TFileStream.Create(filename, fmOpenRead);
  ds := TDecompressionStream.Create(fs);

  fs.Read(data_size, SizeOf(data_size));
  fs.Read(node_index_arr_size, SizeOf(node_index_arr_size));
  UnLoadMemProfile;
  Getmem(MemProfilerData, data_size);
  Getmem(node_index_arr, node_index_arr_size*SizeOf(node_index_arr[0]));
  data_pos := 0;

  if (data_size=ds.Read(MemProfilerData^, data_size)) and
     ((node_index_arr_size*SizeOf(node_index_arr[0])=ds.Read(node_index_arr^, node_index_arr_size*SizeOf(node_index_arr[0])))) then
  begin
    vt.BeginUpdate;

    get_group_index;

    for i:=get_node_count-1 downto 0 do
    begin
      node:=vt.AddChild(nil, get_node);
      vt.HasChildren[node]:=get_node_has_children;

      PPointer(vt.GetNodeData(node))[1]:=Pointer(get_group_index);
      PPointer(vt.GetNodeData(node))[2]:=Pointer(0); // на самом деле это булевый флаг 1 - ноды созданы, 0 - не созданы
    end;

    vt.EndUpdate;
    vt.Header.AutoFitColumns(False);
  end
  else
  begin
    UnLoadMemProfile;
    ShowMessage('Something wrong');
  end;

  ds.Free;
  fs.Free;
end;

procedure TMainForm.UnLoadMemProfile;
begin
  if Assigned(MemProfilerData) then
  begin
    Freemem(MemProfilerData);
    MemProfilerData:=nil;
  end;
  if Assigned(node_index_arr) then
  begin
    Freemem(node_index_arr);
    node_index_arr:=nil;
  end;
end;

end.

