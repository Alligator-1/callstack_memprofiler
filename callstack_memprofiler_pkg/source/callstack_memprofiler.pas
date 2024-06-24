unit callstack_memprofiler;
{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  SysUtils, Generics.Collections
  ,Forms, Controls, Graphics, Classes
  ,laz.VirtualTrees
  ,lnfodwrf
  ;

procedure dumptofile;
procedure ReplaceMemoryManager;
procedure RestoreMemoryManager;
procedure ResetData;

implementation

type
  {TODO: отслеживать как-то AllocMem и GetMem отдельно}
  TInfo = record
    code_addr: CodePointer;
    real_size: SizeInt;
    mem_sum_alloc,
    mem_sum_free: SizeInt;
    max_block_alloc,
    min_block_alloc: Cardinal;
    max_block_free,
    min_block_free: Cardinal;
    count_alloc,
    count_free: Cardinal;
    procedure update(const args: array of const); inline;
    function avg_alloc_block_size: SizeInt; inline;
    function avg_free_block_size: SizeInt; inline;
    function count: Cardinal; inline;
    function mem_size: SizeInt; inline;
  end;

  PCodePointer = ^TCodePointer;
  TCodePointer = record
    next: array of TCodePointer;
    data: TInfo;
  end;
  PCodePointerArray = ^TCodePointerArray;
  TCodePointerArray = array of TCodePointer;
  TStackTreeArray = array of record
    name: String;
    stack_tree: TCodePointerArray;
  end;

  PNodeData = ^TNodeData;
  TNodeData = record
    name: String;
    data: TInfo;
  end;

  TMemUsageMonitor = class(specialize TDictionary<Pointer, SizeInt>)
    function AddPtr(p:pointer; Size:SizeInt):SizeInt; inline;
    function DelPtr(p:pointer):SizeInt; inline;
    function ChgPtr(p:pointer; Size:SizeInt):SizeInt; inline;
  end;

  TEventsHandler = class
    procedure VTOnGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;TextType: TVSTTextType; var CellText: AnsiString);
    procedure VTOnCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
    procedure VTOnFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure VTOnExpanded(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure VTOnDrawText(Sender: TBaseVirtualTree; TargetCanvas: TCanvas;
      Node: PVirtualNode; Column: TColumnIndex; const CellText: String;
      const CellRect: TRect; var DefaultDraw: Boolean);
    procedure FormOnClose(Sender: TObject; var CloseAction: TCloseAction);
  end;

const
  skip_memusage_frames = 3;
  skip_memusage_bottom_frames = 2;

var
  root_stack: TStackTreeArray;
  mem_monitor: TMemUsageMonitor;
  NewMM, OldMM: TMemoryManager;
  f: TForm;
  need_free_form: boolean = False;


procedure collect_callstack(const name: String; const args: array of const; skip_frames: integer = 2; skip_bottom_frames: integer=0);
var
  frames: array [0..255] of codepointer;
  list: PCodePointerArray;
  pcp: PCodePointer;
  root: ^TStackTreeArray;
  i, i2, count: longint;
  found: boolean;
begin
  count:=CaptureBacktrace(skip_frames,255,@frames[0]);

  found:=false;
  root := @root_stack;
  for i:=high(root^) downto low(root^) do
  begin
    found := root^[i].name = name;
    if found then
    begin
      list:=@root^[i].stack_tree;
      Break;
    end;
  end;
  if not found then
  begin
    SetLength(root^, Length(root^)+1);
    root^[high(root^)].name:=name;
    list:=@root^[high(root^)].stack_tree;
  end;

  for i:=count-1-skip_bottom_frames downto 0 do
  begin
    found:=false;
    for i2:=low(list^) to high(list^) do
    begin
      pcp:=@list^[i2];
      found := pcp^.data.code_addr=frames[i];
      if found then Break;
    end;
    if not found then
    begin
      SetLength(list^, length(list^)+1);
      pcp:=@list^[high(list^)];
      pcp^.data.code_addr:=frames[i];
      pcp^.data.min_block_alloc:=High(pcp^.data.min_block_alloc);
      pcp^.data.min_block_free:=High(pcp^.data.min_block_free);
    end;
    pcp^.data.update(args);
    list:=@pcp^.next;
  end;
end;

procedure TInfo.update(const args: array of const);
var
  size: SizeInt;
begin
  size:=args[0].VInt64^;

  if size>0 then
  begin
    inc(count_alloc);
    inc(mem_sum_alloc, size);

    if size>max_block_alloc then max_block_alloc:=size;
    if size<min_block_alloc then min_block_alloc:=size;
  end
  else if size<0 then
  begin
    inc(count_free);
    inc(mem_sum_free, -size);

    if -size>max_block_free then max_block_free:=-size;
    if -size<min_block_free then min_block_free:=-size;
  end else
  begin
    // не знаю надо инкрементить или не надо, пока не решил
    //inc(count_alloc);
    //inc(count_free);

    // Надо подумать как сделать правильно,
    // если расскоментировать то минимальный блок везде почти становится =0
    // но это может быть ошибкой из-за неидальности отслеживания указателей
    //min_block_alloc:=0;
    //min_block_free:=0;
  end;
end;

function TInfo.avg_alloc_block_size: SizeInt;
begin
  if count_alloc>0 then
  begin
    result:=(mem_sum_alloc + (count_alloc div 2)) div count_alloc;
  end else
  begin
    result:=mem_sum_alloc;
  end;
end;
function TInfo.avg_free_block_size: SizeInt;
begin
  if count_free>0 then
  begin
    result:=(mem_sum_free + (count_free div 2)) div count_free;
  end else
  begin
    result:=mem_sum_free;
  end;
end;
function TInfo.count: Cardinal;
begin
  result:=count_alloc+count_free;
end;
function TInfo.mem_size: SizeInt;
begin
  result:=mem_sum_alloc-mem_sum_free;
end;

procedure collect_memusage(mem_size:SizeInt); inline;
begin
  collect_callstack('MemUsage', [mem_size], skip_memusage_frames, skip_memusage_bottom_frames);
end;

function NewGetMem(Size:ptruint):Pointer;
begin
  Result := OldMM.GetMem(Size);

  SetMemoryManager(OldMM);
  collect_memusage(mem_monitor.AddPtr(Result, Size));
  //collect_callstack('NewGetMem',[]);
  SetMemoryManager(NewMM);
end;
function NewFreeMem(p:pointer):ptruint;
begin
  Result := OldMM.FreeMem(p);

  SetMemoryManager(OldMM);
  collect_memusage(mem_monitor.DelPtr(p));
  //collect_callstack('NewFreeMem',[]);
  SetMemoryManager(NewMM);
end;
function NewFreeMemSize(p:pointer;Size:ptruint):ptruint;
begin
  Result := OldMM.FreeMemSize(p,Size);

  SetMemoryManager(OldMM);
  collect_memusage(mem_monitor.DelPtr(p));
  //collect_callstack('NewFreeMemSize',[]);
  SetMemoryManager(NewMM);
end;
function NewAllocMem(Size:ptruint):pointer;
begin
  Result := OldMM.AllocMem(size);

  SetMemoryManager(OldMM);
  collect_memusage(mem_monitor.AddPtr(Result, Size));
  //collect_callstack('NewAllocMem',[]);
  SetMemoryManager(NewMM);
end;
function NewReAllocMem(var p:pointer;Size:ptruint):pointer;
var
  old_p:pointer;
begin
  old_p:=p;
  Result := OldMM.ReAllocMem(p,Size);

  SetMemoryManager(OldMM);
  if old_p<>p then
  begin
    collect_memusage(mem_monitor.DelPtr(old_p));
    collect_memusage(mem_monitor.AddPtr(Result, Size));
  end
  else begin
    collect_memusage(mem_monitor.ChgPtr(old_p, Size));
  end;

  //collect_callstack('NewReallocMem',[]);
  SetMemoryManager(NewMM);
end;

procedure ReplaceMemoryManager;
var
  MM: TMemoryManager;
begin
  GetMemoryManager(MM);
  if MM.AllocMem<>@NewAllocMem then
  begin
    mem_monitor:=TMemUsageMonitor.Create;

    OldMM:=MM;
    NewMM:=MM;

    NewMM.Getmem:=@NewGetMem;
    NewMM.Freemem:=@NewFreeMem;
    NewMM.FreememSize:=@NewFreeMemSize;
    NewMM.AllocMem:=@NewAllocMem;
    NewMM.ReAllocMem:=@NewReAllocMem;

    SetMemoryManager(NewMM);
  end;
end;
procedure RestoreMemoryManager;
var
  MM: TMemoryManager;
begin
  GetMemoryManager(MM);
  if MM.AllocMem=@NewAllocMem then
  begin
    SetMemoryManager(OldMM);
    mem_monitor.Free;
  end;
end;

procedure ResetData;
begin
  SetMemoryManager(OldMM);
  mem_monitor.Clear;
  SetLength(root_stack, 0);
  SetMemoryManager(NewMM);
end;

// Copy from unit "lnfodwrf"
{$push}
{$H-}
function MyDwarfBackTraceStr(addr: CodePointer): shortstring;
var
  hs,
  source  : shortstring;
  line    : longint;
  Store   : TBackTraceStrFunc;
  Success : boolean;
begin
  Success:=false;
  Store := BackTraceStrFunc;
  BackTraceStrFunc := @SysBackTraceStr;
  Success:=GetLineInfo(codeptruint(addr), result, source, line);

  if Success then
  begin
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
  end;
  BackTraceStrFunc := Store;
end;
{$pop}

procedure TEventsHandler.VTOnGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: AnsiString);
const
  null_str = '-';
var
  pnd: PNodeData;
begin
  pnd:=PPointer(Sender.GetNodeData(Node))^;
  if not Assigned(pnd) then
  begin
    CellText:='nil';
    Exit;
  end;
  case Column of
     0: CellText:=IntToHex(PtrUInt(pnd^.data.code_addr)).TrimLeft('0');
     1: CellText:=IntToStr(pnd^.data.count);
     2: CellText:=IntToStr(pnd^.data.mem_size);
     3: CellText:=IntToStr(pnd^.data.real_size);

     4: CellText:=IntToStr(pnd^.data.count_alloc);
     5: if pnd^.data.count_alloc>0 then CellText:=IntToStr(pnd^.data.mem_sum_alloc) else CellText:=null_str;
     6: if pnd^.data.count_alloc>0 then CellText:=IntToStr(pnd^.data.max_block_alloc) else CellText:=null_str;
     7: if pnd^.data.count_alloc>0 then CellText:=IntToStr(pnd^.data.min_block_alloc) else CellText:=null_str;
     8: if pnd^.data.count_alloc>0 then CellText:=IntToStr(pnd^.data.avg_alloc_block_size) else CellText:=null_str;

     9: CellText:=IntToStr(pnd^.data.count_free);
    10: if pnd^.data.count_free>0 then CellText:=IntToStr(-pnd^.data.mem_sum_free) else CellText:=null_str;
    11: if pnd^.data.count_free>0 then CellText:=IntToStr(-pnd^.data.max_block_free) else CellText:=null_str;
    12: if pnd^.data.count_free>0 then CellText:=IntToStr(-pnd^.data.min_block_free) else CellText:=null_str;
    13: if pnd^.data.count_free>0 then CellText:=IntToStr(-pnd^.data.avg_free_block_size) else CellText:=null_str;
    14: begin
      if pnd^.name='' then pnd^.name:=MyDwarfBackTraceStr(pnd^.data.code_addr);
      CellText:=pnd^.name;
    end;
    else CellText:='huh?';
  end;
end;

generic function CompareValue<T>(const a, b: T): Integer; inline;
begin
  Result:=1;
  if a=b then Result:=0
  else if a<b then Result:=-1;
end;

procedure TEventsHandler.VTOnCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
var
  pnd1, pnd2: PNodeData;
begin
  if not Column in [1..13] then Exit;

  pnd1:=PPointer(Sender.GetNodeData(Node1))^;
  pnd2:=PPointer(Sender.GetNodeData(Node2))^;
  if not Assigned(pnd1) or not Assigned(pnd1) then Exit;

  case Column of
     1: Result:=specialize CompareValue<Cardinal>(pnd1^.data.count, pnd2^.data.count);
     2: Result:=specialize CompareValue<SizeInt>(pnd1^.data.mem_size, pnd2^.data.mem_size);
     3: Result:=specialize CompareValue<SizeInt>(pnd1^.data.real_size, pnd2^.data.real_size);

     4: Result:=specialize CompareValue<Cardinal>(pnd1^.data.count_alloc, pnd2^.data.count_alloc);
     5: Result:=specialize CompareValue<SizeInt>(pnd1^.data.mem_sum_alloc, pnd2^.data.mem_sum_alloc);
     6: Result:=specialize CompareValue<Cardinal>(pnd1^.data.max_block_alloc, pnd2^.data.max_block_alloc);
     7: Result:=specialize CompareValue<Cardinal>(pnd1^.data.min_block_alloc, pnd2^.data.min_block_alloc);
     8: Result:=specialize CompareValue<SizeInt>(pnd1^.data.avg_alloc_block_size, pnd2^.data.avg_alloc_block_size);

     9: Result:=specialize CompareValue<Cardinal>(pnd1^.data.count_free, pnd2^.data.count_free);
    10: Result:=specialize CompareValue<SizeInt>(pnd1^.data.mem_sum_free, pnd2^.data.mem_sum_free);
    11: Result:=specialize CompareValue<Cardinal>(pnd1^.data.max_block_free, pnd2^.data.max_block_free);
    12: Result:=specialize CompareValue<Cardinal>(pnd1^.data.min_block_free, pnd2^.data.min_block_free);
    13: Result:=specialize CompareValue<SizeInt>(pnd1^.data.avg_free_block_size, pnd2^.data.avg_free_block_size);
  end;
end;

procedure TEventsHandler.VTOnFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  Dispose(PNodeData(PPointer(Sender.GetNodeData(Node))^));
end;

procedure TEventsHandler.VTOnExpanded(Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  if (Node^.ChildCount=1) and (([vsToggling,vsExpanded] * Node^.FirstChild^.States)=[]) then Sender.ToggleNode(Node^.FirstChild);
  TLazVirtualStringTree(Sender).Header.AutoFitColumns(False);
end;

procedure TEventsHandler.VTOnDrawText(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  const CellText: String; const CellRect: TRect; var DefaultDraw: Boolean);
begin
  case Column of
    1..3: begin
            TargetCanvas.Font.Color:=clBlack;
            if vsSelected in Node^.States then TargetCanvas.Brush.Color:=$A5FFA5 else TargetCanvas.Brush.Color:=$DCFFDC;
            TargetCanvas.Clear;
          end;
    4..8: begin
            TargetCanvas.Font.Color:=clBlack;
            if vsSelected in Node^.States then TargetCanvas.Brush.Color:=$A5A5FF else TargetCanvas.Brush.Color:=$DCDCFF;
            TargetCanvas.Clear;
          end;
    9..13: begin
            TargetCanvas.Font.Color:=clBlack;
            if vsSelected in Node^.States then TargetCanvas.Brush.Color:=$FFA5A5 else TargetCanvas.Brush.Color:=$DCDCDC;
            TargetCanvas.Clear;
          end;
  end;
end;

procedure TEventsHandler.FormOnClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction:=caFree;
  need_free_form:=False;
end;

var
  bad_add_ptr_cnt:integer=0;
  bad_del_ptr_cnt:integer=0;
  bad_cng_ptr_cnt:integer=0;

function TMemUsageMonitor.AddPtr(p: pointer; Size: SizeInt): SizeInt;
begin
  if ContainsKey(p) then
  begin
    inc(bad_add_ptr_cnt);
    Exit(0);
  end;
  Result:=Size;
  Add(p, Size);
end;

function TMemUsageMonitor.DelPtr(p: pointer): SizeInt;
begin
  if not ContainsKey(p) then
  begin
    inc(bad_del_ptr_cnt);
    Exit(0);
  end;
  Result:=-Items[p];
  Remove(p);
end;

function TMemUsageMonitor.ChgPtr(p: pointer; Size: SizeInt): SizeInt;
begin
  if not ContainsKey(p) then
  begin
    inc(bad_cng_ptr_cnt);
    Exit(0);
  end;
  Result:=Size-Items[p];
  AddOrSetValue(p, Size);
end;


procedure dumptofile;
var
  vt: TLazVirtualStringTree;
  i,ii: Integer;
  pn: PVirtualNode;
  root_pnd, pnd: PNodeData;
  stack_index: integer = -1;
  stack: array of record
    val: PCodePointerArray;
    pvn: PVirtualNode;
  end;
  pstack_arr: PCodePointerArray;

  procedure push(const val:PCodePointerArray; const pvn: PVirtualNode);inline;
  begin
    if stack_index<Length(stack) then SetLength(stack, Length(stack)+10);
    inc(stack_index);
    stack[stack_index].val:=val;
    stack[stack_index].pvn:=pvn;
  end;
  procedure pop(out val:PCodePointerArray; out pvn: PVirtualNode);inline;
  begin
    val:=stack[stack_index].val;
    pvn:=stack[stack_index].pvn;
    dec(stack_index);
  end;

begin
  SetMemoryManager(OldMM);

  need_free_form:=True;
  f:=TForm.Create(nil);
  f.OnClose:=@TEventsHandler(nil).FormOnClose;
  vt:=TLazVirtualStringTree.Create(f);
  vt.Parent:=f;
  vt.NodeDataSize:=SizeOf(Pointer);
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
  vt.Align:=alClient;
  vt.OnGetText:=@TEventsHandler(nil).VTOnGetText;
  vt.OnFreeNode:=@TEventsHandler(nil).VTOnFreeNode;
  vt.OnExpanded:=@TEventsHandler(nil).VTOnExpanded;
  vt.OnCompareNodes:=@TEventsHandler(nil).VTOnCompareNodes;
  vt.OnDrawText:=@TEventsHandler(nil).VTOnDrawText;
  f.Show;

  vt.BeginUpdate;
  for i:=Low(root_stack) to High(root_stack) do
  begin
    New(root_pnd);
    FillChar(root_pnd^.data, SizeOf(root_pnd^.data), 0);
    root_pnd^.name:=root_stack[i].name;
    pn:=vt.AddChild(nil, root_pnd);

    push(@root_stack[i].stack_tree, pn);

    while stack_index>=0 do
    begin
      pop(pstack_arr, pn);

      inc(root_pnd^.data.count_alloc);
      inc(root_pnd^.data.mem_sum_alloc,Length(pstack_arr^));

      for ii:=Low(pstack_arr^) to High(pstack_arr^) do
      begin
        New(pnd);
        pnd^.data:=pstack_arr^[ii].data;
        pnd^.name:='';

        if Length(pstack_arr^[ii].next)>0 then
        begin
          push(@pstack_arr^[ii].next, vt.AddChild(pn, pnd));
        end else
        begin
          vt.AddChild(pn, pnd);
        end;
      end;
    end;
  end;
  vt.EndUpdate;
  vt.Header.AutoFitColumns(False);

  SetMemoryManager(NewMM);
end;

procedure ExitProc;
begin
  RestoreMemoryManager;
  if need_free_form then
  begin
    f.Close;
    need_free_form:=False;
  end;
end;

initialization
  AddExitProc(@ExitProc);

end.

