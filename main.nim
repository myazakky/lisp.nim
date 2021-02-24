import tables

import node, functions, parser, evaluation

var environment: Environment
environment = {
  "+": LispNode(kind: OperationLiteral, operation: addition),
  "*": LispNode(kind: OperationLiteral, operation: multiplication),
  "atom": LispNode(kind: OperationLiteral, operation: atom),
  "eq": LispNode(kind: OperationLiteral, operation: eq),
  "car": LispNode(kind: OperationLiteral, operation: car),
  "cdr": LispNode(kind: OperationLiteral, operation: cdr),
  "cons": LispNode(kind: OperationLiteral, operation: cons),

  "define": LispNode(kind: OperationLiteral, operation: define),
  "if": LispNode(kind: OperationLiteral, operation: ifExpression),
  "quote": LispNode(kind: OperationLiteral, operation: quote),
  "lambda": LispNode(kind: OperationLiteral, operation: lambdaExpression),

  "exit": LispNode(kind: OperationLiteral, operation: exit)
}.toTable

stdout.write("👑 Wlecome to Pure Lisp 👑\n")
while true:
  stdout.write("lisp.nim> ")
  var resultNode: LispNode
  (resultNode, environment) = eval(stdin.readLine.parse, environment)
  echo resultNode
