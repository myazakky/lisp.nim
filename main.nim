import os
import tables, sequtils
import strutils

import node, functions, parser, evaluation

var environment: Environment
environment = {
  "+": LispNode(kind: OperationLiteral, operation: addition),
  "-": LispNode(kind: OperationLiteral, operation: subtraction),
  "*": LispNode(kind: OperationLiteral, operation: multiplication),
  "/": LispNode(kind: OperationLiteral, operation: division),
  "atom": LispNode(kind: OperationLiteral, operation: atom),
  "eq": LispNode(kind: OperationLiteral, operation: eq),
  "car": LispNode(kind: OperationLiteral, operation: car),
  "cdr": LispNode(kind: OperationLiteral, operation: cdr),
  "cons": LispNode(kind: OperationLiteral, operation: cons),

  "define": LispNode(kind: OperationLiteral, operation: define),
  "if": LispNode(kind: OperationLiteral, operation: ifExpression),
  "quote": LispNode(kind: OperationLiteral, operation: quote),
  "lambda": LispNode(kind: OperationLiteral, operation: lambdaExpression),

  "display": LispNode(kind: OperationLiteral, operation: display),

  "exit": LispNode(kind: OperationLiteral, operation: exit)
}.toTable

proc repl =
  stdout.write("👑 Wlecome to Pure Lisp 👑\n")
  while true:
    var resultNode: LispNode
    var newEnv = environment

    stdout.write("lisp.nim> ")
    (resultNode, newEnv) = eval(stdin.readLine.parse, environment)

    if newEnv.hasKey("(ERROR)"):
      echo newEnv["(ERROR)"]
    else:
      environment = newEnv
      echo resultNode

proc runProgram(programs: seq[string]) =
  var resultNode: LispNode
  for program in programs:
    (resultNode, environment) = eval(program.parse, environment)

    if environment.hasKey("(ERROR)"): raise newException(Exception, $environment["(ERROR)"])

if paramCount() == 0:
  repl()
else:
  var f = open(paramStr(1), FileMode.fmRead)
  defer: f.close()
  runProgram(f.readAll.split("\n").filterIt(it != ""))
