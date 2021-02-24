import strformat, tables

type
  Environment* = Table[string, LispNode]
  EvaluationResult* = tuple[node: LispNode, env: Environment]

  LispNodeKind* = enum
    IdentifierLiteral
    SymbolLiteral
    IntegerLiteral
    BooleanLiteral
    NilLiteral
    PairLiteral
    OperationLiteral
    Expression
  
  LispNode* = ref LispNodeObj

  LispNodeObj* = object
    case kind*: LispNodeKind
    of IdentifierLiteral:
      identifierValue*: string
    of SymbolLiteral:
      symbolValue*: string
    of IntegerLiteral:
      integerValue*: int
    of BooleanLiteral:
      booleanValue*: bool
    of NilLiteral:
      discard
    of PairLiteral:
      car*: LispNode
      cdr*: LispNode
    of OperationLiteral:
      operation*: proc(e: Environment, n: varargs[LispNode]): EvaluationResult
    of Expression:
      operationLiteral*: LispNode
      arguments*: seq[LispNode]


func `$`*(node: LispNode): string =
    case node.kind
    of IdentifierLiteral: fmt"<Identifer: {node.identifierValue}>"
    of SymbolLiteral: fmt"'{node.symbolValue}"
    of IntegerLiteral: $node.integerValue
    of BooleanLiteral: fmt"#{$node.booleanValue}"
    of NilLiteral: "'()"
    of PairLiteral: fmt"'({$node.car} {$node.cdr})"
    of OperationLiteral: fmt"<operation>"
    of Expression: "exp"
