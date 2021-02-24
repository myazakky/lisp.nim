import sequtils, tables
import strutils

import evaluation, node

func `+`(a, b: LispNode): LispNode =
  LispNode(
    kind: IntegerLiteral,
    integerValue: a.integerValue + b.integerValue
  )

func `*`(a, b: LispNode): LispNode =
  LispNode(
    kind: IntegerLiteral,
    integerValue: a.integerValue * b.integerValue
  )

func addition*(env: Environment, nodes: varargs[LispNode]): EvaluationResult =
  let evaluatedNodes = nodes.map(proc(x: LispNode): LispNode = eval(x, env).node)
  (evaluatedNodes.foldl(
    a + b,
    LispNode(kind: IntegerLiteral, integervalue: 0)),
   env)

func multiplication*(env: Environment, nodes: varargs[LispNode]): EvaluationResult =
  let evaluatedNodes = nodes.map(proc(x: LispNode): LispNode = eval(x, env).node)
  (evaluatedNodes.foldl(
    a * b,
    LispNode(kind: IntegerLiteral, integervalue: 1)),
   env)

func atom*(env: Environment, nodes: varargs[LispNode]): EvaluationResult =
  let evaluatedNodes = nodes.map(proc(x: LispNode): LispNode = eval(x, env).node)

  if evaluatedNodes[0].kind in IdentifierLiteral..NilLiteral:
    (LispNode(kind: BooleanLiteral, booleanValue: true), env)
  else:
    (LispNode(kind: BooleanLiteral, booleanValue: false), env)

func eq*(env: Environment, nodes: varargs[LispNode]): EvaluationResult =
  let evaluatedNodes = nodes.map(proc(x: LispNode): LispNode = eval(x, env).node)

  let first = evaluatedNodes[0]
  if not evaluatedNodes.all(proc(x: LispNode): bool = x.kind == first.kind):
    (LispNode(kind: BooleanLiteral, booleanValue: false), env)
  else:
    let equal = case evaluatedNodes[0].kind
                 of IdentifierLiteral:
                   nodes.all(proc(x: LispNode): bool = x.identifierValue == first.identifierValue)
                 of SymbolLiteral:
                   nodes.all(proc(x: LispNode): bool = x.symbolValue == first.symbolValue)
                 of IntegerLiteral:
                   nodes.all(proc(x: LispNode): bool = x.integerValue == first.integerValue)
                 of BooleanLiteral:
                   nodes.all(proc(x: LispNode): bool = x.booleanValue == first.booleanValue)
                 of NilLiteral: true
                 else: false
    (LispNode(kind: BooleanLiteral, booleanValue: equal), env)

func car*(env: Environment, nodes: varargs[LispNode]): EvaluationResult = 
  let evaluatedNodes = nodes.map(proc(x: LispNode): LispNode = eval(x, env).node)
  (evaluatedNodes[0].car, env)

func cdr*(env: Environment, nodes: varargs[LispNode]): EvaluationResult =
  let evaluatedNodes = nodes.map(proc(x: LispNode): LispNode = eval(x, env).node)
  (evaluatedNodes[0].cdr, env)

func cons*(env: Environment, nodes: varargs[LispNode]): EvaluationResult =
  let evaluatedNodes = nodes.map(proc(x: LispNode): LispNode = eval(x, env).node)
  (LispNode(
    kind: PairLiteral,
    car: evaluatedNodes[0],
    cdr: evaluatedNodes[1]
  ), env)

proc define*(env: Environment, nodes: varargs[LispNode]): EvaluationResult =
  var newEnv = env
  newEnv[nodes[0].identifierValue] = eval(nodes[1], env).node
  (LispNode(kind: NilLiteral), newEnv)

proc ifExpression*(env: Environment, nodes: varargs[LispNode]): EvaluationResult =
  let condition = eval(nodes[0], env).node

  if condition.kind == NilLiteral:
    eval(nodes[2], env)
  elif condition.kind == BooleanLiteral and not condition.booleanValue:
    eval(nodes[2], env)
  else:
    eval(nodes[1], env)

func quote*(env: Environment, nodes: varargs[LispNode]): EvaluationResult =
  (nodes[0], env)

func lambdaExpression*(env: Environment, nodes: varargs[LispNode]): EvaluationResult =
  let expression = nodes[^1]
  let arguments = nodes[0..^2]

  let operation = proc(e: Environment, n: varargs[LispNode]): EvaluationResult =
    var appliedEnv = e

    for index, argument in arguments:
      appliedEnv[argument.identifierValue] = eval(n[index], e).node
    eval(expression, appliedEnv)

  (LispNode(
    kind: OperationLiteral,
    operation: operation
  ), env)

proc display*(env: Environment, n: varargs[LispNode]): EvaluationResult =
  echo n.map(`$`).join("")
  (LispNode(kind: NilLiteral), env)

proc exit*(env: Environment, nodes: varargs[LispNode]): EvaluationResult =
  quit(0)
