import sequtils, tables
import strutils

import evaluation, node

func `+`(a, b: LispNode): LispNode =
  if (a.kind, b.kind) == (FloatLiteral, FloatLiteral):
    return LispNode(kind: FloatLiteral, floatValue: a.floatValue + b.floatValue)
  elif (a.kind, b.kind) == (FloatLiteral, NumberLiteral):
    return LispNode(kind: FloatLiteral, floatValue: a.floatValue + b.topvalue / b.bottomValue)
  elif (a.kind, b.kind) == (NumberLiteral, FloatLiteral):
    return LispNode(kind: FloatLiteral, floatValue: a.topValue / a.bottomValue + b.floatValue)
  elif (a.kind, b.kind) == (NumberLiteral, NumberLiteral):
    return LispNode(
      kind: NumberLiteral,
      topValue: (a.topValue * b.bottomValue) + (a.bottomValue * b.topValue),
      bottomValue: (a.bottomValue * b.bottomValue)
    ).simplifyFraction

func `-`(a, b: LispNode): LispNode =
  if (a.kind, b.kind) == (FloatLiteral, FloatLiteral):
    return LispNode(kind: FloatLiteral, floatValue: a.floatValue - b.floatValue)
  elif (a.kind, b.kind) == (FloatLiteral, NumberLiteral):
    return LispNode(kind: FloatLiteral, floatValue: a.floatValue - b.topvalue / b.bottomValue)
  elif (a.kind, b.kind) == (NumberLiteral, FloatLiteral):
    return LispNode(kind: FloatLiteral, floatValue: a.topValue / a.bottomValue - b.floatValue)
  elif (a.kind, b.kind) == (NumberLiteral, NumberLiteral):
    return LispNode(
      kind: NumberLiteral,
      topValue: (a.topValue * b.bottomValue) - (a.bottomValue * b.topValue),
      bottomValue: (a.bottomValue * b.bottomValue)
    ).simplifyFraction

func `*`(a, b: LispNode): LispNode =
  if (a.kind, b.kind) == (FloatLiteral, FloatLiteral):
    return LispNode(kind: FloatLiteral, floatValue: a.floatValue * b.floatValue)
  elif (a.kind, b.kind) == (FloatLiteral, NumberLiteral):
    return LispNode(kind: FloatLiteral, floatValue: a.floatValue * (b.topvalue / b.bottomValue))
  elif (a.kind, b.kind) == (NumberLiteral, FloatLiteral):
    return LispNode(kind: FloatLiteral, floatValue: (a.topValue / a.bottomValue) * b.floatValue)
  elif (a.kind, b.kind) == (NumberLiteral, NumberLiteral):
    return LispNode(
      kind: NumberLiteral,
      topValue: a.topValue * b.topValue,
      bottomValue: a.bottomValue * b.bottomValue
    ).simplifyFraction

func `/`(a, b: LispNode): LispNode =
  if (b.kind == FloatLiteral and b.floatValue == 0.0) or (b.kind == NumberLiteral and b.topValue == 0):
    return LispNode(kind: Error, message: "Division by zero")

  if (a.kind, b.kind) == (FloatLiteral, FloatLiteral):
    return LispNode(kind: FloatLiteral, floatValue: a.floatValue / b.floatValue)
  elif (a.kind, b.kind) == (FloatLiteral, NumberLiteral):
    return LispNode(kind: FloatLiteral, floatValue: a.floatValue / (b.topvalue / b.bottomValue))
  elif (a.kind, b.kind) == (NumberLiteral, FloatLiteral):
    return LispNode(kind: FloatLiteral, floatValue: (a.topValue / a.bottomValue) / b.floatValue)
  elif (a.kind, b.kind) == (NumberLiteral, NumberLiteral):
    return LispNode(
      kind: NumberLiteral,
      topValue: a.topValue * b.bottomValue,
      bottomValue: a.bottomValue * b.topValue
    ).simplifyFraction

func addition*(env: Environment, nodes: varargs[LispNode]): EvaluationResult =
  let evaluatedNodes = nodes.map(proc(x: LispNode): LispNode = eval(x, env).node)
  (evaluatedNodes.foldl(
    a + b,
    LispNode(kind: NumberLiteral, topValue: 0, bottomValue: 1)),
   env)

func subtraction*(env: Environment, nodes: varargs[LispNode]): EvaluationResult =
  let evaluatedNodes = nodes.map(proc(x: LispNode): LispNode = eval(x, env).node)
  if evaluatedNodes.len == 1:
    ((LispNode(kind: NumberLiteral, topValue: 0, bottomValue: 1) - evaluatedNodes[0]), env)
  else:
    (evaluatedNodes.foldl(a - b), env)

func multiplication*(env: Environment, nodes: varargs[LispNode]): EvaluationResult =
  let evaluatedNodes = nodes.map(proc(x: LispNode): LispNode = eval(x, env).node)
  (evaluatedNodes.foldl(
    a * b,
    LispNode(kind: NumberLiteral, topValue: 1, bottomValue: 1)),
   env)

func division*(env: Environment, nodes: varargs[LispNode]): EvaluationResult =
  let evaluatedNodes = nodes.map(proc(x: LispNode): LispNode = eval(x, env).node)
  if evaluatedNodes.len == 1:
    ((LispNode(kind: NumberLiteral, topValue: 1, bottomValue: 1) / evaluatedNodes[0]), env)
  else:
    (evaluatedNodes.foldl(a / b), env)

func atom*(env: Environment, nodes: varargs[LispNode]): EvaluationResult =
  let evaluatedNodes = nodes.map(proc(x: LispNode): LispNode = eval(x, env).node)

  if evaluatedNodes[0].kind in IdentifierLiteral..NilLiteral:
    (LispNode(kind: BooleanLiteral, booleanValue: true), env)
  else:
    (LispNode(kind: BooleanLiteral, booleanValue: false), env)

proc eq*(env: Environment, nodes: varargs[LispNode]): EvaluationResult =
  let evaluatedNodes = nodes.map(proc(x: LispNode): LispNode = eval(x, env).node)
  let first = evaluatedNodes[0]

  if not evaluatedNodes.all(proc(x: LispNode): bool = x.kind == first.kind):
    (LispNode(kind: BooleanLiteral, booleanValue: false), env)
  else:
    let equal = case evaluatedNodes[0].kind
                 of IdentifierLiteral:
                   evaluatedNodes.all(proc(x: LispNode): bool = x.identifierValue == first.identifierValue)
                 of SymbolLiteral:
                   evaluatedNodes.all(proc(x: LispNode): bool = x.symbolValue == first.symbolValue)
                 of NumberLiteral:
                   evaluatedNodes.all(proc(x: LispNode): bool =
                     x.topValue == first.topValue and
                     x.bottomValue == first.bottomValue)
                 of BooleanLiteral:
                   evaluatedNodes.all(proc(x: LispNode): bool = x.booleanValue == first.booleanValue)
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
  echo n.map(proc(x: LispNode): string = $eval(x, env).node).join("")
  (LispNode(kind: NilLiteral), env)

proc exit*(env: Environment, nodes: varargs[LispNode]): EvaluationResult =
  quit(0)
