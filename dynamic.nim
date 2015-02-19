import tables, hashes
from strutils import parseFloat, `%`, split

type
  DynamicKind {.pure.} = enum
    Num
    String
    List
    Map
  Dynamic* = object
    case kind: DynamicKind
    of DynamicKind.Num:
      numVal: float64
    of DynamicKind.String:
      stringVal: string
    of DynamicKind.List:
      listVal: seq[Dynamic]
    of DynamicKind.Map:
      mapVal: ref Table[Dynamic, Dynamic]


proc hash*(self: Dynamic): THash
proc hash*[K, V](self: ref Table[K, V]): THash =
  result = hash(self.len)
  for key, val in self:
    result = result !& hash(key)
    result = result !& hash(val)
  result = !$result

proc `==`*(a, b: Dynamic): bool =
  if a.kind == b.kind:
    case a.kind
    of DynamicKind.Num:    return a.numVal    == b.numVal
    of DynamicKind.String: return a.stringVal == b.stringVal
    of DynamicKind.List:   return a.listVal   == b.listVal
    of DynamicKind.Map:    return a.mapVal    == b.mapVal
  return false

proc `$`*(self: Dynamic): string =
  case self.kind
  of DynamicKind.Num:
    return $self.numVal
  of DynamicKind.String:
    return "\"" & self.stringVal & "\""
  of DynamicKind.List:
    result = "["
    for v in self.listVal:
      if result.len != 1:
        result.add(", ")
      result.add($v)
    result.add("]")
  of DynamicKind.Map:
    result = "{"
    for key, value in self.mapVal:
      if result.len != 1:
        result.add(", ")
      result.add($key & " : " & $value)
    result.add("}")


proc hash*(self: Dynamic): THash =
  case self.kind
  of DynamicKind.Num:    return hash(self.numVal)
  of DynamicKind.String: return hash(self.stringVal)
  of DynamicKind.List:   return hash(self.listVal)
  of DynamicKind.Map:    return hash(self.mapVal)


proc dyn*[T](val: T): Dynamic =
  when T is Dynamic:
    return val
  elif T is seq:
    result = Dynamic( kind : DynamicKind.List, listVal : @[] )
    for v in val:
      result.listVal.add(v.dyn)
  elif T is (int|int8|int16|int32|int64|uint|uint8|uint16|uint32|uint64) or
       T is (float|float32|float64):
    result = Dynamic( kind : DynamicKind.Num, numVal : float64(val) )
  elif T is (string|char|cstring):
    result = Dynamic( kind : DynamicKind.String, stringVal : $val )
  elif T is object:
    result = Dynamic( kind : DynamicKind.Map, mapVal : newTable[Dynamic, Dynamic]() )
    for name, value in val.fieldPairs:
      result.mapVal[dyn(name)] = dyn(value)
  else:
    {.error: "Dunno what do do with " & val.}

proc asNum*(self: Dynamic): float64 =
  case self.kind
  of DynamicKind.Num:    return self.numVal
  of DynamicKind.String: return parseFloat(self.stringVal)
  else:
    raise newException(ObjectConversionError,
      "Unable to coerce $1 to number" % [$self])

proc asStr*(self: Dynamic): string =
  case self.kind
  of DynamicKind.Num: return $self.numVal
  of DynamicKind.String: return self.stringVal
  else:
    raise newException(ObjectConversionError,
      "Unable to coerce $1 to string" % [$self])

proc `.`*(self: Dynamic, path: string): Dynamic =
  if self.kind == DynamicKind.Map:
    let path = path.split('.')

proc `+`*(a, b: Dynamic): Dynamic = return dyn(a.asNum + b.asNum)
proc `-`*(a, b: Dynamic): Dynamic = return dyn(a.asNum - b.asNum)
proc `*`*(a, b: Dynamic): Dynamic = return dyn(a.asNum * b.asNum)
proc `/`*(a, b: Dynamic): Dynamic = return dyn(a.asNum / b.asNum)
proc `div`*(a, b: Dynamic): Dynamic = return dyn(int(a.asNum / b.asNum))
proc `mod`*(a, b: Dynamic): Dynamic = return dyn(int(a.asNum) mod int(b.asNum))

when isMainModule:
  type Foo = object
    a: int
    b: float

  let x = "123".dyn
  echo(x)
  echo(x + 5.dyn)
  echo(Foo(a: 123, b: 2e53).dyn)
