import strformat, tables

type
  Environment* = Table[string, LispNode]
  EvaluationResult* = tuple[node: LispNode, env: Environment]

  LispNodeKind* = enum
    IdentifierLiteral
    SymbolLiteral
    NumberLiteral
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
    of NumberLiteral:
      topValue*: int
      bottomValue*: int
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
    of NumberLiteral: fmt"{node.topValue}/{node.bottomValue}"
    of BooleanLiteral: fmt"#{$node.booleanValue}"
    of NilLiteral: "'()"
    of PairLiteral: fmt"'({$node.car} {$node.cdr})"
    of OperationLiteral: fmt"<operation>"
    of Expression: "exp"
