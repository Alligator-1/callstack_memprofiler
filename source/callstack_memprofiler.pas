unit callstack_memprofiler;
{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  SysUtils, Generics.Collections, Classes, callstack_memprofiler_common, ZStream;

procedure SaveProfileToFile(filename: string = '');
procedure ReplaceMemoryManager;
function IsMemoryManagerReplaced: Boolean;
procedure RestoreMemoryManager;
procedure ResetData;

implementation

type
  {TODO: отслеживать как-то AllocMem и GetMem отдельно}

  generic TNode<T> = record
  type
    PNode = ^TNode;
  public
    node_data: T;
    child_nodes: array of TNode;
    function add_child_node: PNode; inline;
  end;
  // сохранение тоже выделить в TNode, и возможно загрузку тоже и вынести в отдельный юнит

  PMemProfilerNode = ^TMemProfilerNode;
  TMemProfilerNode = specialize TNode<TInfo>;
  HMemProfilerNodeHelper = record helper for TMemprofilerNode
    function find_child_node(addr: CodePointer): PMemProfilerNode;// inline; // bug in FPC #40865
  end;


  TMemUsageMonitor = class(specialize TDictionary<Pointer, SizeInt>)
    function AddPtr(p:pointer; Size:SizeInt):SizeInt; inline;
    function DelPtr(p:pointer):SizeInt; inline;
    function ChgPtr(p:pointer; Size:SizeInt):SizeInt; inline;
  end;

const
  skip_memusage_frames = 3;
  skip_memusage_bottom_frames = 2;

var
  root_node: TMemProfilerNode;
  mem_monitor: TMemUsageMonitor;
  NewMM, OldMM: TMemoryManager;

function TNode.add_child_node: PNode;
begin
  SetLength(child_nodes, length(child_nodes)+1);
  Result:=@child_nodes[high(child_nodes)];
end;

function HMemProfilerNodeHelper.find_child_node(addr: CodePointer): PMemProfilerNode;
var
  i: longint;
begin
  for i:=low(child_nodes) to high(child_nodes) do
    if child_nodes[i].node_data.code_addr=addr then Exit(@child_nodes[i]);
  Result:=nil
end;

procedure collect_callstack(mem_size: SizeInt; skip_frames: integer = 2; skip_bottom_frames: integer=0);
var
  frames: array [0..255] of codepointer;
  node, found_node: PMemProfilerNode;
  i, count: longint;
begin
  count:=CaptureBacktrace(skip_frames,255,@frames[0]);

  node:=@root_node;

  for i:=count-1-skip_bottom_frames downto 0 do
  begin
    found_node:=node^.find_child_node(frames[i]);

    if Assigned(found_node) then
      begin
        node:=found_node
      end
    else
      begin
        node:=node^.add_child_node;
        node^.node_data.init(frames[i]);
      end;

    node^.node_data.update(mem_size);
  end;
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

function NewGetMem(Size:ptruint):Pointer;
begin
  Result := OldMM.GetMem(Size);

  SetMemoryManager(OldMM);
  collect_callstack(mem_monitor.AddPtr(Result, Size), skip_memusage_frames, skip_memusage_bottom_frames);
  SetMemoryManager(NewMM);
end;
function NewFreeMem(p:pointer):ptruint;
begin
  Result := OldMM.FreeMem(p);

  SetMemoryManager(OldMM);
  collect_callstack(mem_monitor.DelPtr(p), skip_memusage_frames, skip_memusage_bottom_frames);
  SetMemoryManager(NewMM);
end;
function NewFreeMemSize(p:pointer;Size:ptruint):ptruint;
begin
  Result := OldMM.FreeMemSize(p,Size);

  SetMemoryManager(OldMM);
  collect_callstack(mem_monitor.DelPtr(p), skip_memusage_frames, skip_memusage_bottom_frames);
  SetMemoryManager(NewMM);
end;
function NewAllocMem(Size:ptruint):pointer;
begin
  Result := OldMM.AllocMem(size);

  SetMemoryManager(OldMM);
  collect_callstack(mem_monitor.AddPtr(Result, Size), skip_memusage_frames, skip_memusage_bottom_frames);
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
    collect_callstack(mem_monitor.DelPtr(old_p), skip_memusage_frames, skip_memusage_bottom_frames);
    collect_callstack(mem_monitor.AddPtr(Result, Size), skip_memusage_frames, skip_memusage_bottom_frames);
  end
  else begin
    collect_callstack(mem_monitor.ChgPtr(old_p, Size), skip_memusage_frames, skip_memusage_bottom_frames);
  end;

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

function IsMemoryManagerReplaced: Boolean;
var
  MM: TMemoryManager;
begin
  GetMemoryManager(MM);
  Result:=MM.AllocMem=@NewAllocMem;
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
  SetLength(root_node.child_nodes, 0);
  SetMemoryManager(NewMM);
end;


type
  TNodeIndexStruct = record
    node: PMemProfilerNode;
    group_index: LongInt;
    class function init(const node_:PMemProfilerNode; const group_index_:LongInt): TNodeIndexStruct; static; inline;
  end;
class function TNodeIndexStruct.init(const node_:PMemProfilerNode; const group_index_:LongInt): TNodeIndexStruct;
begin
  Result.node:=node_;
  Result.group_index:=group_index_;
end;

procedure SaveProfileToFile(filename: string = '');
type
  TListUInt64 = specialize TList<UInt64>;
  TNodeStack = specialize TStack<TNodeIndexStruct>;
var
  i: Integer;
  node_index: TNodeIndexStruct;
  node_stack: TNodeStack;
  index_arr: TListUInt64;

  group_index_counter: LongInt;
  MemoryManagerWasReplaced: Boolean;
  data_size: LongInt = 0;
  ms,cs: TStream;

  procedure write_header; inline;
  var
    cnt: LongInt;
  begin
    ms.Seek(0, soBeginning);
    ms.Write(data_size, SizeOf(data_size));
    cnt:=index_arr.Count;
    ms.Write(cnt, SizeOf(cnt));
    ms.Seek(0, soEnd);
  end;
  procedure write_nodes_index; inline;
  begin
    cs.Write(index_arr.List[0], SizeOf(index_arr.List[0])*index_arr.Count);
  end;
  procedure write_node_count(const count: integer); inline;
  begin
    cs.Write(count, SizeOf(count));
    inc(data_size, SizeOf(count));
  end;
  procedure write_node_group_index(const index: LongInt); inline;
  begin
    cs.Write(index, SizeOf(index));
    inc(data_size, SizeOf(index));
  end;
  procedure write_node_has_children(const has_children: Boolean); inline;
  begin
    cs.Write(has_children, SizeOf(has_children));
    inc(data_size, SizeOf(has_children));
  end;
  procedure write_node(const data: TInfo); inline;
  begin
    cs.Write(data, SizeOf(TInfo));
    inc(data_size, SizeOf(TInfo));
  end;
begin
  MemoryManagerWasReplaced:=IsMemoryManagerReplaced;

  if MemoryManagerWasReplaced then SetMemoryManager(OldMM);

  if filename='' then filename:=FormatDateTime('yyyymmddHHMMSS', Now)+'.memprof';
  ms := TMemoryStream.Create;
  cs := TCompressionStream.Create(clfastest, ms);


  index_arr:=TListUInt64.Create;
  node_stack:=TNodeStack.Create;

  group_index_counter:=0;

  write_header;

  node_stack.Push(TNodeIndexStruct.init(@root_node, group_index_counter));
  while node_stack.Count>0 do
  begin
    node_index:=node_stack.Pop;

    if node_index.group_index>=index_arr.Count then index_arr.Count:=node_index.group_index+1;
    index_arr[node_index.group_index]:=data_size;
    write_node_group_index(node_index.group_index);
    write_node_count(Length(node_index.node^.child_nodes));
    for i:=Low(node_index.node^.child_nodes) to High(node_index.node^.child_nodes) do
    begin
      write_node(node_index.node^.child_nodes[i].node_data);
      write_node_has_children(Length(node_index.node^.child_nodes[i].child_nodes)>0);

      inc(group_index_counter);
      write_node_group_index(group_index_counter);

      node_stack.Push(TNodeIndexStruct.init(@node_index.node^.child_nodes[i], group_index_counter));
    end;
  end;

  write_nodes_index;

  write_header;

  cs.Free;
  TMemoryStream(ms).SaveToFile(filename);
  ms.Free;

  index_arr.Free;
  node_stack.Free;

  if MemoryManagerWasReplaced then SetMemoryManager(NewMM);
end;

procedure ExitProc;
begin
  RestoreMemoryManager;
end;

initialization
  AddExitProc(@ExitProc);

end.

