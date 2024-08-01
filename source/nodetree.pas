unit nodetree;
{$mode ObjFPC}{$H+}
{$modeswitch advancedrecords}

interface

type
  TEmptyRecord = record end;
  TLazyLoadData = record
    loaded: Boolean;
    offset: UInt64;
  end;

  // переделать в интерфейс? "-": лишние данные, "+": ?
  TAbstractDataIO = class abstract
    procedure ReadData(var data; size: UInt64); virtual; abstract;
    procedure WriteData(const data; size: UInt64); virtual; abstract;
  end;


  // 1. подумать о переходе на массив указателей (тогда Node'у к тому же можно будет сделать классом)
  //    нужен бенчмарк (взять с реального проекта количество чтений и кол-во добавлений)
  //    или... подумать как сделать ещё один слой и скрыть это за ним
  //    слой должен быть таким, чтобы не влиял на производительность
  //
  // 2. т.к. кол-во чтений на вскидку должно быть от 10 раз (а сокрее и от 100) преобладать над кол-вом добавлений
  //    поэтому и есть сомнения что будет быстрее... за счёт локальности данных в памяти массива
  //
  // 3. Если делать динамическое выделение - то рассмотреть chunked array для хранения данных
  //    а если ещё удастся скрыть это всё за слоем - то получится элегантно переключаться между всеми тремя вариантами:
  //    а) как сейчас - перевыделение массива с данными
  //    б) перевыделение массива с индексом данных в chunked array
  //    в) перевыделение массива с указателями в динамически выделенной памяти
  //
  // 4. Найти соотношение на реальном проекте количества нод к количеству массивов = средняя длина массива -> сделать чуть
  //    более умное выделение или, может быть перейти на TList<>?(больше расход данных на каждый массив(экземплярные данные) -
  //    оценить, на фоне того, что он резервирует данные может это вообще не существенно...,
  //    кроме того нужно будет сделать свой цикл освобождения всех данных дерева... чето пока лень,
  //    нужны тесты/бенчмарки - от этого и плясать)
  generic TAbstractNodeTree<TNodeDataType, TNodeDataIO, TAdditionalData> = class abstract
  type
    PNodeDataType = ^TNodeDataType;
    TNodeDataType_ = TNodeDataType; /////// УДАЛИТЬ ПОТОМ?!! Когда протестирую Viewer
    PNode = ^TNode;
    TNode = record
      node_data: TNodeDataType;
      additional_data: TAdditionalData;
      child_nodes: array of TNode;
    end;
  public
    root_node: TNode;
    class function add_child_node(node: PNode): PNode; static; inline;
    destructor Destroy; override;
    procedure Clear;
    procedure SaveToFile(filename: string = '');
    procedure LoadFromFile(filename: string);
  end;

  generic TNodeTree<TNodeDataType, TNodeDataIO> = class(specialize TAbstractNodeTree<TNodeDataType, TNodeDataIO, TEmptyRecord>);

  generic TLazyLoadNodeTree<TNodeDataType, TNodeDataIO> = class(specialize TAbstractNodeTree<TNodeDataType, TNodeDataIO, TLazyLoadData>)

  end;

  generic TAutoGrowStack<T> = record
  type
    T_ = T;
  public
    stack: array of T;
    count: UInt32;
    procedure set_by_index(index: UInt32; value: T); inline;
    property getset[index: UInt32]:T write set_by_index; default;
  end;

implementation

{ TAbstractDataIO }

procedure TAbstractNodeTree.Clear;
begin
  SetLength(root_node.child_nodes, 0);
end;

procedure TAbstractNodeTree.SaveToFile(filename: string);
var
  NodeDataIO: TNodeDataIO;
  child_count: UInt32;

  procedure SaveNodeRecursive(var node: TNode);
  var
    i: Int32;
  begin
    NodeDataIO.WriteData(node.node_data, SizeOf(TNodeDataType));

    child_count:=Length(node.child_nodes);
    NodeDataIO.WriteData(child_count, SizeOf(child_count));

    for i:=Low(node.child_nodes) to High(node.child_nodes) do
      SaveNodeRecursive(node.child_nodes[i]);
  end;
begin
  NodeDataIO:=TNodeDataIO.Create(filename);

  SaveNodeRecursive(root_node);

  NodeDataIO.Free;
end;

procedure TAbstractNodeTree.LoadFromFile(filename: string);
var
  NodeDataIO: TNodeDataIO;
  child_count: UInt32;

  procedure LoadNodeRecursive(var node: TNode);
  var
    i: Int32;
  begin
    NodeDataIO.ReadData(node.node_data, SizeOf(TNodeDataType));
    NodeDataIO.ReadData(child_count, SizeOf(child_count));
    SetLength(node.child_nodes, child_count);

    for i:=Low(node.child_nodes) to High(node.child_nodes) do
      LoadNodeRecursive(node.child_nodes[i]);
  end;
begin
  Clear;

  NodeDataIO:=TNodeDataIO.Create(filename);

  LoadNodeRecursive(root_node);

  NodeDataIO.Free;
end;

destructor TAbstractNodeTree.Destroy;
begin
  Clear;
  inherited Destroy;
end;

class function TAbstractNodeTree.add_child_node(node: PNode): PNode;
begin
  with node^ do
  begin
    SetLength(child_nodes, Length(child_nodes)+1);
    Result:=@child_nodes[high(child_nodes)];
  end;
end;

procedure TAutoGrowStack.set_by_index(index: UInt32; value: T);
begin
  if index>high(stack) then SetLength(stack, Length(stack)+10000);
  if index>=count then count:=index+1; // можно заменить на inc(count), но так надёжнее
  stack[index]:=value;
end;

end.

