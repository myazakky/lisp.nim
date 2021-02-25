import tables

import node

proc eval*(node: LispNode, env: Environment): EvaluationResult =
  case node.kind
  of IdentifierLiteral:
    if not env.hasKey(node.identifierValue):
      result = (node, env)
    else:
      result = (env[node.identifierValue], env)
  of SymbolLiteral..OperationLiteral:
    result = (node, env)
  of Expression:
    let operate = eval(node.operationLiteral, env).node.operation
    result = operate(env, node.arguments)
