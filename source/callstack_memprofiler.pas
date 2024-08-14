unit callstack_memprofiler;
{$mode objfpc}{$H+}

interface

uses
  Classes, Generics.Collections, callstack_memprofiler_common, nodetree, nodetreedataio;

procedure SaveProfileToFile(filename: string = '');
procedure LoadProfileFromFile(filename: string);
procedure ReplaceMemoryManager;
function IsMemoryManagerReplaced: Boolean;
procedure RestoreMemoryManager;
procedure ResetData;

implementation

type
  {TODO: отслеживать как-то AllocMem и GetMem отдельно}

  TNodeComparator = class sealed
    class function IsEqual(node_data: Pointer; data: Pointer): Boolean; static; inline;
  end;

  TMemProfilerNodeTree = specialize TSimpleArrayNodeTree<TMemProfilerNodeData, TDefaultNodeDataIO, TNodeComparator>;
  //TMemProfilerNodeTree = specialize TPointerArrayNodeTree<TMemProfilerNodeData, TDefaultNodeDataIO, TNodeComparator>;

  TMemUsageMonitor = class sealed(specialize TDictionary<Pointer, SizeInt>)
    function AddPtr(p:pointer; Size:SizeInt):SizeInt; inline;
    function DelPtr(p:pointer):SizeInt; inline;
    function ChgPtr(p:pointer; Size:SizeInt):SizeInt; inline;
  end;

const
  skip_memusage_frames = 3;
  skip_memusage_bottom_frames = 2;

var
  memprofiler_tree: TMemProfilerNodeTree;
  mem_monitor: TMemUsageMonitor;
  NewMM, OldMM: TMemoryManager;

procedure collect_callstack(mem_size: SizeInt; skip_frames: integer = 2; skip_bottom_frames: integer=0);
var
  frames: array [0..255] of codepointer;
  node, found_node: TMemProfilerNodeTree.PNode;
  i, count: longint;
begin
  count:=CaptureBacktrace(skip_frames,255,@frames[0]);

  node:=@memprofiler_tree.root_node;

  for i:=count-1-skip_bottom_frames downto 0 do
  begin
    found_node:=TMemProfilerNodeTree.find_child_node(node, frames[i]);

    if Assigned(found_node) then
      begin
        node:=found_node
      end
    else
      begin
        node:=TMemProfilerNodeTree.add_child(node);
        node^.node_data.init(frames[i]);
      end;

    node^.node_data.update(mem_size);
  end;
end;

var
  bad_add_ptr_cnt:integer=0;
  bad_del_ptr_cnt:integer=0;
  bad_cng_ptr_cnt:integer=0;

class function TNodeComparator.IsEqual(node_data: Pointer; data: Pointer): Boolean;
begin
  Result:=PMemProfilerNodeData(node_data)^.code_addr = data;
end;

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
    memprofiler_tree:=TMemProfilerNodeTree.Create;
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
    memprofiler_tree.Free;
    mem_monitor.Free;
  end;
end;

procedure ResetData;
begin
  SetMemoryManager(OldMM);
  memprofiler_tree.Clear;
  mem_monitor.Clear;
  SetMemoryManager(NewMM);
end;


procedure SaveProfileToFile(filename: string = '');
var
  MemoryManagerWasReplaced: Boolean;
begin
  MemoryManagerWasReplaced:=IsMemoryManagerReplaced;

  if MemoryManagerWasReplaced then SetMemoryManager(OldMM);

  memprofiler_tree.SaveToFile(filename);

  if MemoryManagerWasReplaced then SetMemoryManager(NewMM);
end;

procedure LoadProfileFromFile(filename: string);
var
  MemoryManagerWasReplaced: Boolean;
begin
  MemoryManagerWasReplaced:=IsMemoryManagerReplaced;

  if MemoryManagerWasReplaced then SetMemoryManager(OldMM);

  memprofiler_tree.LoadFromFile(filename);

  if MemoryManagerWasReplaced then SetMemoryManager(NewMM);
end;

procedure ExitProc;
begin
  RestoreMemoryManager;
end;


initialization
  AddExitProc(@ExitProc);

end.

