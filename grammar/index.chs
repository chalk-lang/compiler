///
  This module defines the syntax of the Chalk programming language.
  
  Convention: nonterminals must generate no leading or trailing whitespace. Two
  exceptions are the root nonterminal and the single line comment (which includes
  the trailing space, including the newline).
///

pub []Expr chalkModule =
  [ Maybe([ Match(comment, "moduleDoc") ]),
    // This is here to set the location of error of missing moduleDoc.
    Match(firstSpace),
    Repeat(
      [ Match(importDecl, "imports"), Space() ],
    ),
    Match(expressionList, "defs"),
  ];

pub []Expr firstSpace = [ Space() ];

pub []Expr comment =
  [ Or(
      [ Match(singlelineComment, "comment") ],
      [ Match(multilineComment, "comment") ],
    ),
  ];

pub []Expr singlelineComment =
  [ Text("//"),
    And(
      [ Not(Text("/")) ],
      [ Space(), Match(singlelineDoc, "chalkDoc") ],
    ),
    Space("\n"),
  ];

pub []Expr multilineComment =
  [ Text("///"),
    Space(),
    Match(multilineDoc, "chalkDoc") ])
    Space()
    Text("///")
  ];

pub []Expr importDecl =
  [ Text("import")
  , Space()
  , Or(
      [ Match(identifier, "imports", { type: IdentifierType.upperCase }) ],
      [ Match(objectLit, "imports",
          { type: Destructuring.Typed.never }
        )
      ],
    )
  , Space()
  , Text("from")
  , Space()
  , Match(stringLit, "path")
  , Space()
  , Text(";")
  ];

pub []Expr identifier = [ Or([ Match(lIdentifier) ], [ Match(uIdentifier) ]) ];

pub []Expr lIdentifier = [ Text.az, Match(identifierRest) ];
pub []Expr uIdentifier = [ Text.AZ, Match(identifierRest) ];

pub []Expr identifierRest = [ Repeat(Or([ Text.az ], [ Text.AZ ], [ Text.dg ])) ];

pub []Expr expressionList =
  [ Repeat(
      [ Or(
          [ Match(expressionSC, "expr", { rightmost: tt, top: tt }),
            Space,
            Text(";"),
          ],
          [ Match(expressionCB, "expr", { rightmost: tt, top: tt }) ],
        ),
      ],
    ),
    Or(
      [ Match(expressionSC, "expr", { rightmost: tt }),
        Maybe(
          Space,
          Text(";"),
        ),
      ],
      [ Match(expressionCB, "expr", { rightmost: tt }) ],
    ),
  ];

pub []Expr expression =
  [ Or(
      [ Match(expressionSC, "expr", { rightmost: "rightmost" }) ],
      [ Match(expressionCB, "expr", { rightmost: "rightmost" }) ],
    )
  ];

// An expression that, if child of a block, must end with a semicolon.
pub []Expr expressionSC =
  [ Or(
      [ Match(conditionalOp, "expr", { rightmost: "rightmost" }) ],
      [ Match(elvisOp, "expr", { rightmost: "rightmost" }) ],
      [ Match(fnDef, "fnDef") ],
      [ Match(classDef, "classDef") ],
      [ Match(traitDef, "traitDef") ],
    ),
  ];

pub []Expr conditionalOp =
  [ Or(
      [ Match(orOp, "cond", { rightmost: ff }),
        Space(),
        Text("?"),
        Space(),
        Match(expression, "then", { rightmost: tt }),
        Space(),
        Text(":"),
        Space(),
        Match(conditionalOp, "else", { rightmost: "rightmost" }),
      ],
      [ Match(orOp, "expr", { rightmost: "rightmost" }) ],
    ),
  ];

pub []Expr elvisOp =
  [ Or(
      [ Match(orOp, "left", { rightmost: ff }),
        Space(),
        Text("?:"),
        Space(),
        Match(elvisOp, "then", { rightmost: "rightmost" }),
      ],
      [ Match(orOp, "expr", { rightmost: "rightmost" }) ],
    ),
  ];

pub []Expr orOp =
  [ Or(
      [ Match(andOp, "left", { rightmost: ff }),
        Space(),
        Text("||"),
        Space(),
        Match(orOp, "rigt", { rightmost: "rightmost" }),
      ],
      [ Match(andOp, "expr", { rightmost: "rightmost" }) ],
    ),
  ];

pub []Expr andOp =
  [ Or(
      [ Match(compareOp, "left", { rightmost: ff }),
        Space(),
        Text("&&"),
        Space(),
        Match(andOp, "rigt", { rightmost: "rightmost" }),
      ],
      [ Match(compareOp, "expr", { rightmost: "rightmost" }) ],
    ),
  ];

pub []Expr compareOp =
  [ Or(
      [ Repeat(
          [ [ Match(spaceshipOp, "cmp", { rightmost: ff }),
              Space(),
              Or(
                [ Match(compareLt, "op") ],
                [ Match(compareLts, "op") ],
                [ Match(compareGt, "op") ],
                [ Match(compareGts, "op") ],
                [ Match(compareEq, "op") ],
                [ Match(compareDt, "op") ],
              ),
            ],
          ],
          1,
          null,
        ),
        Space(),
        Match(spaceshipOp, "cmp", { rightmost: "rightmost" }),
        Space(),
        ),
      ],
      [ Match(spaceshipOp, "expr", { rightmost: "rightmost" }) ],
    )
  ];

pub []Expr compareLt = [ Text("<=") ];
pub []Expr compareLts = [ Text("<") ];
pub []Expr compareGt = [ Text(">=") ];
pub []Expr compareGts = [ Text(">") ];
pub []Expr compareEq = [ Text("==") ];
pub []Expr compareDt = [ Text("!=") ];

pub []Expr spaceshipOp =
  [ Or(
      [ Match(monoidOp, "left", { rightmost: ff }),
        Space(),
        Text("<=>"),
        Space(),
        Match(monoidOp, "rigt", { rightmost: "rightmost" }),
      ],
      [ Match(monoidOp, "expr", { rightmost: "rightmost" }) ],
    ),
  ];

pub []Expr monoidOp =
  [ Or(
      [ Match(moduloOp, "left", { rightmost: ff }),
        Space(),
        Text("++"),
        Space(),
        Match(monoidOp, "rigt", { rightmost: "rightmost" }),
      ],
      [ Match(moduloOp, "expr", { rightmost: "rightmost" }) ],
    ),
  ];

pub []Expr moduloOp =
  [ Or(
      [ Match(addSubOp, "left", { rightmost: ff }),
        Space(),
        Text("%"),
        Space(),
        Match(addSubOp, "rigt", { rightmost: "rightmost" }),
      ],
      [ Match(addSubOp, "expr", { rightmost: "rightmost" }) ],
    ),
  ];

pub []Expr addSubOp =
  [ Or(
      [ Match(mulDivOp, "left", { rightmost: ff }),
        Space(),
        Or(
          [ Match(addSubAdd, "op") ],
          [ Match(addSubSub, "op") ],
          [ Match(addSubAddMod, "op") ],
          [ Match(addSubSubMod, "op") ],
        ),
        Space(),
        Match(addSubOp, "rigt", { rightmost: "rightmost" }),
      ],
      [ Match(mulDivOp, "expr", { rightmost: "rightmost" }) ],
    ),
  ];

pub []Expr addSubAdd = [ Text("+") ];
pub []Expr addSubSub = [ Text("-") ];
pub []Expr addSubAddMod = [ Text("+%") ];
pub []Expr addSubSubMod = [ Text("-%") ];

pub []Expr mulDivOp =
  [ Or(
      [ Match(powOp, "left", { rightmost: ff }),
        Space(),
        Or(
          [ Match(mulDivMul, "op") ],
          [ Match(mulDivDiv, "op") ],
          [ Match(mulDivMulMod, "op") ],
        ),
        Space(),
        Match(mulDivOp, "rigt", { rightmost: "rightmost" }),
      ],
      [ Match(powOp, "expr", { rightmost: "rightmost" }) ],
    ),
  ];

pub []Expr mulDivMul = [ Text("*") ];
pub []Expr mulDivDiv = [ Text("/") ];
pub []Expr mulDivMod = [ Text("*%") ];

pub []Expr powOp =
  [ Or(
      [ Match(unaryLeftOp, "left", { rightmost: ff }),
        Space(),
        Text("**"),
        Space(),
        Match(unaryLeftOp, "rigt", { rightmost: "rightmost" }),
      ],
      [ Match(unaryLeftOp, "expr") ],
      [ Match(true, "rightmost"), Match(extraExpr, "expr") ],
    ),
  ];

pub []Expr extraExpr =
  [ Or(
      [ Match(returnExpr, "expr") ],
      [ Match(breakExpr, "expr") ],
      [ Match(comptimeExpr, "expr") ],
      [ Match(constExpr, "expr") ],
      [ Match(assignOp, "expr") ],
    ),
  ];

pub []Expr returnExpr =
  [ Text("return"),
    Match(namedControlFlow),
    Maybe(
      Space(),
      Match(expression, "expr", { rightmost: tt })
    ),
  ];

pub []Expr breakExpr =
  [ Text("break"),
    Match(namedControlFlow),
    Maybe(
      Space(),
      Match(expression, "expr", { rightmost: tt }),
    ),
  ];

pub []Expr namedControlFlow = [ Text("-"), Match(lIdentifier, "name") ];

pub []Expr comptimeExpr =
  [ Text("comptime"),
    Space(),
    Match(expression, "expr", { rightmost: tt }),
  ];

pub []Expr constExpr =
  [ Text("const"), Space(),
    Match(expression, "expr", { rightmost: tt }),
  ];

pub []Expr assignOp =
  [ Match(unaryRightOp, "left"),
    Space(),
    Or(
      [ Match(asgn, "op") ],
      [ Match(asgnAdd, "op") ],
      [ Match(asgnSub, "op") ],
      [ Match(asgnMul, "op") ],
      [ Match(asgnDiv, "op") ],
      [ Match(asgnMod, "op") ],
      [ Match(asgnPow, "op") ],
    ),
    Space(),
    Match(expression, "rigt", { rightmost: tt }),
  ];

pub []Expr asgn = [ Text("=") ];
pub []Expr asgnAdd = [ Text("+=") ];
pub []Expr asgnSub = [ Text("-=") ];
pub []Expr asgnMul = [ Text("*=") ];
pub []Expr asgnDiv = [ Text("/=") ];
pub []Expr asgnMod = [ Text("%=") ];
pub []Expr asgnPow = [ Text("**=") ];

pub []Expr unaryLeftOp =
  [ Or(
      [ Or(
          [ Match(unaryLeftAwait) ],
          [ Match(unaryLeftNowait) ],
          [ Match(unaryLeftIgnore) ],
          [ Match(unaryLeftNegation) ],
        ),
        Space(),
        Match(unaryLeftOp, "sub"),
      ],
      [ Match(unaryRightOp, "sub") ],
    ),
  ];

pub []Expr unaryLeftAwait = [ Text("await") ];
pub []Expr unaryLeftAwait = [ Text("nowait") ];
pub []Expr unaryLeftAwait = [ Text("ignore") ];
pub []Expr unaryLeftAwait = [ Text("!") ];

// asdf

pub []Expr unaryRightOp =
  [ Or(
      [ Match(unaryRightOp, "sub"),
        Space(),
        Or(
          [ Match(memberAccess, "op"), Match(lIdentifier) ],
          [ Match(index, "op"), Match(expression, "index"), Text("]") ],
        ),
      ],
      [ Match(lIdentifier, "lIdentifier") ],
      [ Match(type, "type") ],
      [ Match(fnCall, "fnCall") ],
      [ Match(blockExpr, "blockExpr") ],
      [ Match(switchExpr, "switchExpr") ],
      [ Match(forExpr, "forExpr") ],
      [ Match(continueExpr, "continueExpr") ],
      [ Match(numberLit, "numberLit") ],
      [ Match(stringLit, "stringLit") ],
      [ Match(varDef, "varDef") ],
      [ Match(arrayLit, "arrayLit") ],
      [ Match(tupleLit, "tupleLit") ],
      [ Match(objectLit, "objectLit") ],
      [ Match(setLit, "setLit") ],
    ),
  ];

pub []Expr memberAccess = [ Text(".") ];
pub []Expr index = [ Text("[") ];

pub []Expr type = [ Text("[") ];

pub []Expr continueExpr = [ Text("continue"), Match(namedControlFlow) ];

// An definition that ends with a curly brace and must not end with a semicolon.
pub []Expr expressionCB =
  [ Or(
      [ Match(fnDef, "expr") ],
    )
  ];

// Old code below
pub []Expr definitions =
  [ Repeat(
      [ Or(
          [ Match(objectDestructuring, "definitions"
              { typed: Destructuring.Typed.never }
            )
          , Space("")
          , Text(";")
          ],
          [ Match(variableDefinition, "definitions")
          , Space("")
          , Text(";")
          ],
          [ Match(classDefinition, "definitions", ) ],
          [ Match(traitDefinition, "definitions", ) ],
          [ Match(functionDefinition, "definitions") ],
        )
      ],
      Space("\n"),
      StartSpace(),
      Space("\n"),
    )
  ];

pub []Expr variableDefinition =
    [ Match(type, "type")
    , Space(" ")
    , Match(identifier, "name")
    , Or(
        [ [ Space(" "), Text("="), MSpace(" ", 4, [ Match(expression, "init") ]) ]
        , [ Equals("init", null) ]
        ]
      );
    ];
}

pub []Expr classDefinition =
    [ Text("class")
    , Space(" ")
    , Match(uIdentifier, "name")
    , Maybe(
        [ Space("")
        , Text("<")
        , Space("")
        , Repeat(
            [ Match(parameter, "params") ],
            [ Text(","), MSpace(" ", 6) ]
          )
        , Text(">")
        ]
      )
    , Maybe(
        [ Space(" ")
        , Text(":")
        , Space(" ")
        , Repeat([ Match(Identifier, "extends") ], [ Text(","), Space("\n") ])
        ]
      )
    , Maybe(
        [ Space(" ")
        , Text("friend")
        , Space(" ")
        , Repeat([ Match(Identifier, "friends") ], [ Text(","), Space("\n") ])
        ]
      )
    , Space(" ")
    , Text("{")
    , MSpace("\n", 2,
        [ Repeat(
            [ StartSpace()
            , Or(
                [ Match(classDefinition, "members") ],
                [ Match(traitDefinition, "members") ],
                [ Match(functionDefinition, "members") ],
                [ Match(variableDefinition, "members") ],
                [ Match(destructuringDefinition, "members") ],
              )
            ],
            [ Space("\n"), StartSpace(), Space("\n") ],
          )
        ]
      )
    , Space("\n")
    , StartSpace()
    , Text("}"
    ];
}

pub []Expr traitDefinition =
    [ Text("trait")
    , Space(" ")
    , Match(uIdentifier, "name")
    , Maybe(
        [ Space("")
        , Text("<")
        , Space("")
        , Repeat(
            [ Match(parameter, "params") ],
            [ Text(","), MSpace(" ", 6) ]
          )
        , Text(">")
        ]
      )
    , Maybe(
        [ Space(" ")
        , Text(":")
        , Space(" ")
        , Repeat([ Match(Identifier, "extends") ], [ Text(","), Space("\n") ])
        ]
      )
    , Maybe(
        [ Space(" ")
        , Text("friend")
        , Space(" ")
        , Repeat([ Match(Identifier, "friends") ], [ Text(","), Space("\n") ])
        ]
      )
    , Space(" ")
    , Text("{")
    , MSpace("\n", 2,
        [ Repeat(
            [ StartSpace()
            , Or(
                [ Match(classDefinition, "members") ],
                [ Match(traitDefinition, "members") ],
                [ Match(functionDefinition, "members") ],
                [ Match(functionDeclaration, "declarations") ],
              )
            ],
            [ Space("\n"), StartSpace(), Space("\n") ],
          )
        ]
      )
    , Space("\n")
    , StartSpace()
    , Text("}"
    ];

pub []Expr functionSignature =
    [ Match(type, "returnType")
    , Text("(")
    , Releat(
        [ Match(parameter, "params") ],
        [ Text(","), Space(" ") ],
      )
    , Text(")")
    ];

pub []Expr functionDefinition =
    [ Match(functionSignature)
    , Or(
        [ Text("=>"), Match(expression, "body") ],
        [ Match(block, "body") ],
      )
    ];

pub []Expr functionDeclaration =
    [ Match(functionSignature), Space(""), Text(";") ];

pub []Expr typeUnion = [ Match(typeUTerm), Text("|"), Match(typeUTerm) ];

pub []Expr typeUTerm =
    [ Or(
        
      )
    ];

pub []Expr typeIntersection =
    [
    ];

pub []Expr identifier =
  Bool hasName;
  Type type;
  
  enum Type { object, type, ctt };
  
  static const []Expr =
      OneOf(
        [ [ Equals(Identifier::iType, Type.ctt,
              [ OneOf(
                  [ [ Text("class") ], [ Text("trait") ], [ Text("type") ] ]
                )
              ],
            )
        , [ Equals( Identifier::iType, Type.type, [ OneOf("A-Z") ], [ OneOf("a-z") ] )
          , Repeat( OneOf("a-zA-Z0-9") ) // That's right, NO UNDERSCORE. (Death to snake_case!)
          ]
        ]
      );
}

pub []Expr typedIdentifier =
  Type type;
  Identifier name;
  
  static const []Expr =
      [ Match(TypedIdentifier::type, [ (Identifier::isType, true) ])
      , Space(" ")
      , Match(TypedIdentifier::type, [ (Identifier::isType, TODO) ])
      ];
}

pub []Expr type =
    [
    ];

// Both generic and function parameter
pub []Expr parameter =
    [
    ];

pub []Expr objectDestructuring =
    [
    ];

pub []Expr arrayDestructuring =
    [
    ];

pub []Expr stringLit =

// First part of variable declaration
pub []Expr type =
    [
    ];

pub []Expr todo =
    [
    ];