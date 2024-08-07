unit callstack_memprofiler_common;
{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

type
  PMemProfilerNodeData = ^TMemProfilerNodeData;
  TMemProfilerNodeData = packed record
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
    procedure init(addr: CodePointer); inline;
    procedure update(mem_size: SizeInt); inline;
    function avg_alloc_block_size: SizeInt; inline;
    function avg_free_block_size: SizeInt; inline;
    function count: Cardinal; inline;
    function mem_size: SizeInt; inline;
  end;

implementation

procedure TMemProfilerNodeData.init(addr: CodePointer);
begin
  code_addr:=addr;
  min_block_alloc:=High(min_block_alloc);
  min_block_free:=High(min_block_free);
end;

procedure TMemProfilerNodeData.update(mem_size: SizeInt);
begin
  if mem_size>0 then
  begin
    inc(count_alloc);
    inc(mem_sum_alloc, mem_size);

    if mem_size>max_block_alloc then max_block_alloc:=mem_size;
    if mem_size<min_block_alloc then min_block_alloc:=mem_size;
  end
  else if mem_size<0 then
  begin
    inc(count_free);
    inc(mem_sum_free, -mem_size);

    if -mem_size>max_block_free then max_block_free:=-mem_size;
    if -mem_size<min_block_free then min_block_free:=-mem_size;
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

function TMemProfilerNodeData.avg_alloc_block_size: SizeInt;
begin
  if count_alloc>0 then
  begin
    result:=(mem_sum_alloc + (count_alloc div 2)) div count_alloc;
  end else
  begin
    result:=mem_sum_alloc;
  end;
end;

function TMemProfilerNodeData.avg_free_block_size: SizeInt;
begin
  if count_free>0 then
  begin
    result:=(mem_sum_free + (count_free div 2)) div count_free;
  end else
  begin
    result:=mem_sum_free;
  end;
end;

function TMemProfilerNodeData.count: Cardinal;
begin
  result:=count_alloc+count_free;
end;

function TMemProfilerNodeData.mem_size: SizeInt;
begin
  result:=mem_sum_alloc-mem_sum_free;
end;

end.

