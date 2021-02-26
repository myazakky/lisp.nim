import tables

import node

proc eval*(node: LispNode, env: Environment): EvaluationResult =
  case node.kind
  of IdentifierLiteral:
    if not env.hasKey(node.identifierValue):
      var erroredEnv = env
      erroredEnv["(ERROR)"] = LispNode(kind: Error, message: node.identifierValue & ": undefined")
      result = (LispNode(kind: NilLiteral), erroredEnv)
    else:
      result = (env[node.identifierValue], env)
  of SymbolLiteral..Error:
    result = (node, env)
  of Expression:
    let operate = eval(node.operationLiteral, env).node.operation
    result = operate(env, node.arguments)
