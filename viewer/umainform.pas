unit umainform;
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Generics.Collections, laz.VirtualTrees, nodetree, nodetreedataio, callstack_memprofiler_common, math,
  FpDbgLoader, FpDbgDwarf, FpDbgInfo, FpDbgDwarfDataClasses, FpdMemoryTools, DbgIntfBaseTypes
  ;

type
  TAddrNameDict = specialize TDictionary<Pointer, String>;

  TMemProfilerTree = specialize TPointerArrayNodeTree<TMemProfilerNodeData, TDefaultNodeDataIO, TAbstractNodeComparator>;

  { TMainForm }

  TMainForm = class(TForm)
    btnLoadExecutable: TButton;
    btnLoadMemprofile: TButton;
    IdleTimer1: TIdleTimer;
    vt: TLazVirtualStringTree;
    OpenDialog: TOpenDialog;
    procedure btnLoadExecutableClick(Sender: TObject);
    procedure btnLoadMemprofileClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of string);
    procedure IdleTimer1Timer(Sender: TObject);
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
    MemProfilerTree: TMemProfilerTree;
  public
    procedure LoadNode(const node: PVirtualNode);
    procedure LoadMemProfile(filename: string);
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
  AddrNameDict.Free;
  MemProfilerTree.Free;
end;

procedure TMainForm.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  vt.NodeDataSize:=SizeOf(Pointer);

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

  MemProfilerTree := TMemProfilerTree.Create;

  DwarfInfo:=nil;
{$IF DECLARED(TFpDbgMemModel)}
  MemModel:=nil;
{$ENDIF}
end;

procedure TMainForm.FormDropFiles(Sender: TObject; const FileNames: array of string);
var
  i: integer;
begin
  for i:=0 to Min(1, High(FileNames)) do
  begin
    case LowerCase(ExtractFileExt(FileNames[i])) of
      '.memprof': LoadMemProfile(FileNames[i]);
      '.exe': OpenDWARF(FileNames[i]);
    end;
  end;
end;

procedure TMainForm.IdleTimer1Timer(Sender: TObject);
begin
  TIdleTimer(Sender).Enabled:=False;
  vt.Header.AutoFitColumns(False);
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
  if a>b then Result:=1
  else if a<b then Result:=-1
  else Result:=0;
end;

procedure TMainForm.vtCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
var
  pndt1, pndt2: ^TMemProfilerTree.TNodeDataType;
begin
  if not Column in [1..13] then Exit;

  pndt1:=@TMemProfilerTree.PNode(PPointer(Sender.GetNodeData(Node1))^)^;
  pndt2:=@TMemProfilerTree.PNode(PPointer(Sender.GetNodeData(Node2))^)^;

  case Column of
     1: Result:=specialize CompareValue<Cardinal>(pndt1^.count, pndt2^.count);
     2: Result:=specialize CompareValue<SizeInt>(pndt1^.mem_size, pndt2^.mem_size);
     3: Result:=specialize CompareValue<SizeInt>(pndt1^.real_size, pndt2^.real_size);

     4: Result:=specialize CompareValue<Cardinal>(pndt1^.count_alloc, pndt2^.count_alloc);
     5: Result:=specialize CompareValue<SizeInt>(pndt1^.mem_sum_alloc, pndt2^.mem_sum_alloc);
     6: Result:=specialize CompareValue<Cardinal>(pndt1^.max_block_alloc, pndt2^.max_block_alloc);
     7: Result:=specialize CompareValue<Cardinal>(pndt1^.min_block_alloc, pndt2^.min_block_alloc);
     8: Result:=specialize CompareValue<SizeInt>(pndt1^.avg_alloc_block_size, pndt2^.avg_alloc_block_size);

     9: Result:=specialize CompareValue<Cardinal>(pndt1^.count_free, pndt2^.count_free);
    10: Result:=specialize CompareValue<SizeInt>(pndt1^.mem_sum_free, pndt2^.mem_sum_free);
    11: Result:=specialize CompareValue<Cardinal>(pndt1^.max_block_free, pndt2^.max_block_free);
    12: Result:=specialize CompareValue<Cardinal>(pndt1^.min_block_free, pndt2^.min_block_free);
    13: Result:=specialize CompareValue<SizeInt>(pndt1^.avg_free_block_size, pndt2^.avg_free_block_size);
  end;
end;

procedure TMainForm.vtDrawText(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; const CellText: String; const CellRect: TRect; var DefaultDraw: Boolean);
begin
  TargetCanvas.Font.Color:=clBlack;
end;

procedure TMainForm.vtExpanded(Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  with Sender do
    if (ChildCount[Node]=1) and (([vsToggling,vsExpanded] * GetFirstChild(Node)^.States)=[]) then ToggleNode(GetFirstChild(Node));
  IdleTimer1.Enabled:=True;
end;

procedure TMainForm.vtExpanding(Sender: TBaseVirtualTree; Node: PVirtualNode; var Allowed: Boolean);
begin
  with Sender do
    if HasChildren[Node] and (GetFirstChild(Node)=nil) then LoadNode(Node);
end;

procedure TMainForm.vtGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
const
  null_str = '-';
var
  pndt: ^TMemProfilerTree.TNodeDataType;
begin
  pndt:=@TMemProfilerTree.PNode(PPointer(Sender.GetNodeData(Node))^)^.node_data;

  case Column of
     0: CellText:=IntToHex(PtrUInt(pndt^.code_addr)).TrimLeft('0');
     1: CellText:=UIntToStr(pndt^.count);
     2: CellText:=IntToStr(pndt^.mem_size);
     3: CellText:=IntToStr(pndt^.real_size);

     4: CellText:=UIntToStr(pndt^.count_alloc);
     5: if pndt^.count_alloc>0 then CellText:=IntToStr(pndt^.mem_sum_alloc) else CellText:=null_str;
     6: if pndt^.count_alloc>0 then CellText:=UIntToStr(pndt^.max_block_alloc) else CellText:=null_str;
     7: if pndt^.count_alloc>0 then CellText:=UIntToStr(pndt^.min_block_alloc) else CellText:=null_str;
     8: if pndt^.count_alloc>0 then CellText:=IntToStr(pndt^.avg_alloc_block_size) else CellText:=null_str;

     9: CellText:=UIntToStr(pndt^.count_free);
    10: if pndt^.count_free>0 then CellText:='-'+IntToStr(pndt^.mem_sum_free) else CellText:=null_str;
    11: if pndt^.count_free>0 then CellText:='-'+UIntToStr(pndt^.max_block_free) else CellText:=null_str;
    12: if pndt^.count_free>0 then CellText:='-'+UIntToStr(pndt^.min_block_free) else CellText:=null_str;
    13: if pndt^.count_free>0 then CellText:='-'+IntToStr(pndt^.avg_free_block_size) else CellText:=null_str;
    14: CellText:=GetDWARFInfoByAddress(pndt^.code_addr);
    else CellText:='huh?';
  end;
end;

procedure TMainForm.LoadNode(const node: PVirtualNode);
var
  i: Integer;
  pvn: PVirtualNode;
  pnd: TMemProfilerTree.PNode;
begin
  pnd:=@TMemProfilerTree.PNode(PPointer(vt.GetNodeData(node))^)^.node_data;

  vt.BeginUpdate;

  for i:=0 to TMemProfilerTree.get_child_count(pnd)-1 do
  begin
    pvn:=vt.AddChild(node, TMemProfilerTree.get_child(pnd, i));
    vt.HasChildren[pvn]:=TMemProfilerTree.get_child_count(TMemProfilerTree.get_child(pnd, i))<>0;
  end;

  vt.EndUpdate;
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

    addr_info.ReleaseReference;

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
  pvn: PVirtualNode;
begin
  AddrNameDict.Clear;
  vt.Clear;

  MemProfilerTree.LoadFromFile(filename);

  pvn:=vt.AddChild(nil, @MemProfilerTree.root_node);
  vt.HasChildren[pvn]:=TMemProfilerTree.get_child_count(@MemProfilerTree.root_node)<>0;

  vt.Header.AutoFitColumns(False);
end;


end.

