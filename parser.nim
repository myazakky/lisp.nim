import sequtils, strutils
import strformat, re

import node

type
  TokenTreeKind = enum
    Branch
    Leaf
  TokenTree = object
    symbol: string
    case kind*: TokenTreeKind
    of Branch: children: seq[TokenTree]
    else: discard

func `$`*(tree: TokenTree): string =
  if tree.kind == Leaf:
    tree.symbol
  else:
    let children = tree.children.map(`$`).join(" ")
    fmt"({tree.symbol} {children})"

func halfBlacketAt(expression: string): int =
  var leftBracketCount = 1

  for readAt, character in expression.pairs:
    if character == '(':
      leftBracketCount.inc
    elif character == ')' and (leftBracketCount > 0):
      leftBracketCount.dec
      if leftBracketCount == 0: return readAt + 1
    else: continue

func readWord(expression: string): string =
  for character in expression:
    if character in ['(', ')', ' ']:
      break
    else:
      result &= character

func tokenize*(expression: string): TokenTree =
  if expression.len > 2 and expression[0..2] == "'()":
    return TokenTree(kind: Branch, symbol: ".", children: @[])
  if expression.len > 2 and expression[0..1] == "'(":
    result = TokenTree(kind: Branch)
    result.symbol = "."
    let misic = tokenize(expression[1..^1])
    result.children = concat(@[TokenTree(kind: Leaf, symbol: misic.symbol)], misic.children)
    return

  if expression[0] != '(':
    return TokenTree(kind: Leaf, symbol: readWord(expression))

  var skipTo: int
  let peeled = expression[1..^2]
  result = TokenTree(kind: Branch)

  for readAt, character in peeled.pairs:
    if readAt < skipTo:
      continue
    elif character == ' ' or character == ')': 
      continue
    elif character == '(':
      skipTo = halfBlacketAt(peeled[(readAt + 1)..^1]) + readAt
      result.children.add(tokenize(peeled[readAt..skipTo]))
    elif character == '\'' and peeled[readAt + 1] == '(':
      skipTo = halfBlacketAt(peeled[(readAt + 2)..^1]) + readAt + 1
      let misic = tokenize(peeled[(readAt + 1)..skipTo])
      if misic.symbol == "":
        result.children.add(TokenTree(kind: Branch, symbol: ".", children: @[]))
      else:
        result.children.add(TokenTree(
          kind: Branch,
          symbol: ".",
          children: concat(@[TokenTree(kind: Leaf, symbol: misic.symbol)], misic.children)
        ))
    elif readAt == 0:
      let word = readWord(peeled[readAt..^1])
      skipTo = word.len
      result.symbol = word
    else:
      let word = readWord(peeled[readAt..^1])
      skipTo = word.len + readAt
      result.children.add(
        TokenTree(
          kind: Leaf,
          symbol: word
        )
      )

func parse*(tree: TokenTree): LispNode =
  if tree.kind == Leaf:
    if tree.symbol.match(re"^-??\d+$"):
      LispNode(
        kind: NumberLiteral,
        topValue: tree.symbol.parseInt,
        bottomValue: 1
      )
    elif tree.symbol in ["true", "false"]:
      LispNode(kind: BooleanLiteral, booleanValue: tree.symbol.parseBool)
    elif tree.symbol.match(re"^'\w+$"):
      LispNode(kind: SymbolLiteral, symbolValue: tree.symbol[1..^1])
    else:
      LispNode(kind: IdentifierLiteral, identifierValue: tree.symbol)
  elif tree.symbol == "." and tree.children.len == 0:
    LispNode(kind: NilLiteral)
  elif tree.symbol == "." and tree.children.len == 1:
    LispNode(
      kind: PairLiteral,
      car: tree.children[0].parse,
      cdr: LispNode(kind: NilLiteral)
    )
  elif tree.symbol == ".":
    LispNode(
      kind: PairLiteral,
      car: tree.children[0].parse,
      cdr: TokenTree(
        kind: Branch,
        symbol: ".",
        children: tree.children[1..^1]
      ).parse
    )
  elif tree.symbol == "":
    LispNode(
      kind: Expression,
      operationLiteral: tree.children[0].parse,
      arguments: tree.children[1..^1].map(parse)
    )    
  else:
    LispNode(
      kind: Expression,
      operationLiteral: LispNode(kind: IdentifierLiteral, identifierValue: tree.symbol),
      arguments: tree.children.map(parse)
    )

proc parse*(expression: string): LispNode =
  expression.tokenize.parse
