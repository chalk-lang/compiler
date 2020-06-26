///
  This module defines the syntax of the Chalk programming language.
  
  Convention: nonterminals must generate no leading or trailing whitespace. Two
  exceptions are the root nonterminal and the single line comment (which includes
  the trailing space, including the newline).
///

// TODO import { Grammar } from regerex;
type Grammar = (String, []Expr);

let specialChars = CharClass('()[]{}<>,.!@#$%^&*;:\'"\\|/?`~');

let space = Or(
  [ Repeat(CharClass(' \n\t'), 1) ],
  [ Before(specialChars) ],
  [ After(specialChars) ],
);

export class ChalkScript {
  Comment moduleDoc;
  []Import imports;
  Expressions defs;
  
  static rule = [
    Maybe([ space, Match(Comment, This.moduleDoc) ]),
    space,
    Repeat(
      [ Match(Import, This.imports), space ],
    ),
    Match(Expressions, This.defs, { moduleTop: true }),
    space,
  ];
}

export class Comment {
  SinglelineComment|MultilineComment comment;
  
  static rule = [
    Or(
      [ Match(SinglelineComment, This.comment) ],
      [ Match(MultilineComment, This.comment) ],
    ),
  ];
}

export class SinglelineComment {
  ChalkDoc content;
  
  static rule = [
    Text('//'),
    And(
      [ CharClass('', '\n/'), Repeat([ CharClass('', '\n') ]) ],
      [ space, Match(ChalkDoc, This.chalkDoc), space, ],
    ),
    Text('\n'),
  ],
}

export class MultilineComment {
  ChalkDoc content;
  
  static rule = [
    Text('///'),
    space,
    Match(MultilineDoc, This.chalkDoc),
    space,
    Text('///'),
  ],
}

class Import {
  ValueStruct value;
  StringLiteral path;
  
  static rule = [
    Text('import'),
    space,
    Match(ValueStruct, This.value, { restricted: true }),
    space,
    Text('from'),
    space,
    Match(StringLiteral, This.path, { singleQuotes: true }),
    space,
    Text(";"),
  ],
}

export class Expressions {
  Bool topLevel;
  Bool moduleTop;
  QMarkOp|Export expr;
  
  static rule = [
    Repeat(
      [ Or(
          [ Match(QMarkOp, { moduleTop: This.moduleTop, semicolonRequired: true }), space, Text(';') ],
          [ Match(QMarkOp, { moduleTop: This.moduleTop, semicolonRequired: false }),
            Or(
              [ space, Text(';') ],
              [ After([ space, Text('\n'), space, Text('\n') ]) ]
            ),
          ],
          [ Equals(This.moduleTop, true), Match(Export, This.expr) ],
        ),
      ],
    ),
    space,
    Or(
      [ Match(QMarkOp, { moduleTop: This.moduleTop, semicolonRequired: true }),
        Equal(This.moduleTop, true, [ space, Text(';') ], [ Maybe([ space, Text(';') ]) ]),
      ],
      [ Match(QMarkOp, { moduleTop: This.moduleTop, semicolonRequired: false }),
        Maybe([ space, Text(';') ]),
      ],
    ),
  ],
}

export class Export {
  ValueStruct value;
  StringLiteral path;
  
  static rule = [
    Text('export'),
    space,
    Match(ValueStruct, This.value),
    space,
    Text('from'),
    space,
    Match(StringLiteral, This.path),
    space,
    Text(';'),
  ];
}

export class QMarkOp {
  Bool moduleTop;
  Bool rightmost;
  Bool semicolonRequired;
  
  ?OrOp cond;
  ?QMarkOp then;
  ?QMarkOp else;
  
  ?CondOr next;
  
  static rules = [
    Or(
      [ Equal({ semicolonRequired: true }),
        Match(OrOp, This.cond, { moduleTop: false, rightmost: false }),
        space,
        Text('?'),
        space,
        Match(QMarkOp, This.then, { moduleTop: false, rightmost: true }),
        space,
        Text(':'),
        space,
        Match(QMarkOp, This.else, { moduleTop: false, rightmost: true }),
      ],
      [ Match(CondOp, This.next,
          { semicolonRequired: This.semicolonRequired,
            moduleTop: This.moduleTop,
            rightmost: This.rightmost,
          },
        ),
      ],
    ),
  ];
}

export final trait OpEnum : Enum {
  // Distinct for all instances.
  String op where All OpEnum a, OpEnum b: a.op == b.op -> a == b;
  
  This(String);
  
  static enum Cond : OpEnum {
    or('||');
    and('&&');
    
    String op;
    
    This(_op);
  }
  
  static enum Or : OpEnum {
    or('|');
    
    String op;
    
    This(_op);
  }

  static enum And : OpEnum {
    and('&');
    
    String op;
    
    This(_op);
  }

  static enum Order : OpEnum {
    lt('<');
    le('<=');
    eq('==');
    ne('!=');
    ge('>=');
    gt('>');
    
    String op;
    
    This(_op);
  }

  static class Is : OpEnum {
    is('is');
    
    String op;
    
    This(_op);
  }

  static enum Spaceship : OpEnum {
    cmp('<=>');
    
    String op;
    
    This(_op);
  }

  static enum Magma : OpEnum {
    pp('++');
    
    String op;
    
    This(_op);
  }
}

trait OpFlags {
  Bool moduleTop;
  Bool rightmost;
  Bool semicolonRequired;
}

class Operator[
  type Op : OpEnum,
  type Left : OpFlags,
  type Right: OpFlags,
  type Next : OpFlags,
] : OpFlags {
  Op op;
  Bool moduleTop;
  Bool rightmost;
  Bool semicolonRequired;
  
  ?Left left;
  ?Right right;
  ?Next next
  
  static rules = [
    Or(
      [ Match(Left, This.left, { moduleTop: false, rightmost: false }),
        space,
        Or(
          ...Op.values.map(v => [ Equal(This.op, v.op), Text(v.op) ]),
        ),
        space,
        Match(Right, This.right, { moduleTop: false, rightmost: This.rightmost }),
      ],
      [ Match(Next, This.next,
          { semicolonRequired: This.semicolonRequired,
            moduleTop: This.moduleTop,
            rightmost: This.rightmost,
          },
        ),
      ],
    ),
  ];
}

export class CondOp = Operator[OpEnum.Cond, OrOp, CondOp, OrOp],
export class OrOp = Operator[OpEnum.Or, AndOp, OrOp, AndOp],
export class AndOp = Operator[OpEnum.And, OrderOrIsOp, AndOp, OrderOrIsOp],

export class OrderOrIs: OpFlags {
  Bool moduleTop;
  Bool rightmost;
  Bool semicolonRequired;
  
  OrderOp|IsOs op;
  
  static rules = [
    Or(
      Match(OrderOp, This.op,
        { moduleTop: This.moduleTop,
          rightmost: This.rightmost,
          semicolonRequired: This.semicolonRequired,
        },
      ),
      Match(IsOp, This.op,
        { moduleTop: This.moduleTop,
          rightmost: This.rightmost,
          semicolonRequired: This.semicolonRequired,
        },
      ),
    ),
  ];
}

export class OrderOp : OpFlags {
  Bool moduleTop;
  Bool rightmost;
  Bool semicolonRequired;
  
  []SpaceshipOp ops;
  
  static rules = [
    Or(
      [ Repeat(
          [ Match(SpaceshipOp, This.ops, { moduleTop: false, rightmost: false }),
            space,
            Or(
              ...OpEnum.Order.values.map(v => [ Equal(This.op, v.op), Text(v.op) ]),
            ),
            space,
          ],
        ),
        Match(SpaceshipOp, This.ops, { moduleTop: false, rightmost: This.rightmost }),
      ],
      [ Match(SpaceshipOp, This.ops,
          { moduleTop: This.moduleTop, rightmost: This.rightmost },
        ),
      ],
    ),
  ];
}

export class IsOp = Operator[OpEnum.Is, SpaceshipOp, SpaceshipOp, SpaceshipOp];
export class SpaceshipOp = Operator[OpEnum.Spaceship, MagmaOp, MagmaOp, MagmaOp];
export class MagmaOp = Operator[OpEnum.Magma, ModOp, MagmaOp, ModOp];
export class ModOp = Operator[OpEnum.Mod, AddOp, ModOp, AddOp];
export class MulOp = Operator[OpEnum.Mul, MulOp, AddOp, MulOp];
export class PowOp = Operator[OpEnum.Pow, PowOp, MulOp, PowOp];
export class ElvisOp = Operator[OpEnum.Elvis, UnaryLeftOp, ElvisOp, UnaryLeftOp];

// TODO 'continue' keyword, asdf
enum UnaryLeftEnum {
  ret('return');
  bre('break');
  ign('ignore');
  awa('await');
  now('nowait');
  neg('!');
  
  String str;
  
  This(_str);
}

export class UnaryLeftOp {
  
}

export class ValueStruct',
    { Bool restricted },
    [ Or(
        [ Match('UIdentifier', 'value') ],
        [ Match('Object', 'value',
            { type: Destructuring.Typed.never },
          ),
        ],
      ),
    ],
  ),
  ( 'Identifier',
    [ Or(
        [ Match('LIdentifier') ],
        [ Match('UIdentifier') ],
      ),
    ],
  ),
  ( 'LIdentifier',
    [ And(
        [ Not(keywords) ]
        [ Grammar.asciiAzLower, Repeat([ Grammar.asciiAlphaNum ]) ],
      ),
    ],
  ),
  ( 'UIdentifier',
    [ And(
        [ Not(keywords) ]
        [ Grammar.asciiAzUpper, Repeat([ Grammar.asciiAlphaNum ]) ],
      ),
    ],
  ),
];

pub []Expr expressionList =
  [ Repeat(
      [ Or(
          [ Match(expressionSC, 'expr', { rightmost: tt, top: tt }),
            Space,
            Text(";"),
          ],
          [ Match(expressionCB, 'expr', { rightmost: tt, top: tt }) ],
        ),
      ],
    ),
    Or(
      [ Match(expressionSC, 'expr', { rightmost: tt }),
        Maybe(
          Space,
          Text(";"),
        ),
      ],
      [ Match(expressionCB, 'expr', { rightmost: tt }) ],
    ),
  ];

pub []Expr expression =
  [ Or(
      [ Match(expressionSC, 'expr', { rightmost: 'rightmost' }) ],
      [ Match(expressionCB, 'expr', { rightmost: 'rightmost' }) ],
    )
  ];

// An expression that, if child of a block, must end with a semicolon.
pub []Expr expressionSC =
  [ Or(
      [ Match(conditionalOp, 'expr', { rightmost: 'rightmost' }) ],
      [ Match(elvisOp, 'expr', { rightmost: 'rightmost' }) ],
      [ Match(fnDef, 'fnDef') ],
      [ Match(classDef, 'classDef') ],
      [ Match(traitDef, 'traitDef') ],
    ),
  ];

pub []Expr conditionalOp =
  [ Or(
      [ Match(orOp, 'cond', { rightmost: ff }),
        space,
        Text("?"),
        space,
        Match(expression, 'then', { rightmost: tt }),
        space,
        Text(":"),
        space,
        Match(conditionalOp, 'else', { rightmost: 'rightmost' }),
      ],
      [ Match(orOp, 'expr', { rightmost: 'rightmost' }) ],
    ),
  ];

pub []Expr elvisOp =
  [ Or(
      [ Match(orOp, 'left', { rightmost: ff }),
        space,
        Text("?:"),
        space,
        Match(elvisOp, 'then', { rightmost: 'rightmost' }),
      ],
      [ Match(orOp, 'expr', { rightmost: 'rightmost' }) ],
    ),
  ];

pub []Expr orOp =
  [ Or(
      [ Match(andOp, 'left', { rightmost: ff }),
        space,
        Text("||"),
        space,
        Match(orOp, 'rigt', { rightmost: 'rightmost' }),
      ],
      [ Match(andOp, 'expr', { rightmost: 'rightmost' }) ],
    ),
  ];

pub []Expr andOp =
  [ Or(
      [ Match(compareOp, 'left', { rightmost: ff }),
        space,
        Text("&&"),
        space,
        Match(andOp, 'rigt', { rightmost: 'rightmost' }),
      ],
      [ Match(compareOp, 'expr', { rightmost: 'rightmost' }) ],
    ),
  ];

pub []Expr compareOp =
  [ Or(
      [ Repeat(
          [ [ Match(spaceshipOp, 'cmp', { rightmost: ff }),
              space,
              Or(
                [ Match(compareLt, 'op') ],
                [ Match(compareLts, 'op') ],
                [ Match(compareGt, 'op') ],
                [ Match(compareGts, 'op') ],
                [ Match(compareEq, 'op') ],
                [ Match(compareDt, 'op') ],
              ),
            ],
          ],
          1,
          null,
        ),
        space,
        Match(spaceshipOp, 'cmp', { rightmost: 'rightmost' }),
        space,
        ),
      ],
      [ Match(spaceshipOp, 'expr', { rightmost: 'rightmost' }) ],
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
      [ Match(monoidOp, 'left', { rightmost: ff }),
        space,
        Text("<=>"),
        space,
        Match(monoidOp, 'rigt', { rightmost: 'rightmost' }),
      ],
      [ Match(monoidOp, 'expr', { rightmost: 'rightmost' }) ],
    ),
  ];

pub []Expr monoidOp =
  [ Or(
      [ Match(moduloOp, 'left', { rightmost: ff }),
        space,
        Text("++"),
        space,
        Match(monoidOp, 'rigt', { rightmost: 'rightmost' }),
      ],
      [ Match(moduloOp, 'expr', { rightmost: 'rightmost' }) ],
    ),
  ];

pub []Expr moduloOp =
  [ Or(
      [ Match(addSubOp, 'left', { rightmost: ff }),
        space,
        Text("%"),
        space,
        Match(addSubOp, 'rigt', { rightmost: 'rightmost' }),
      ],
      [ Match(addSubOp, 'expr', { rightmost: 'rightmost' }) ],
    ),
  ];

pub []Expr addSubOp =
  [ Or(
      [ Match(mulDivOp, 'left', { rightmost: ff }),
        space,
        Or(
          [ Match(addSubAdd, 'op') ],
          [ Match(addSubSub, 'op') ],
          [ Match(addSubAddMod, 'op') ],
          [ Match(addSubSubMod, 'op') ],
        ),
        space,
        Match(addSubOp, 'rigt', { rightmost: 'rightmost' }),
      ],
      [ Match(mulDivOp, 'expr', { rightmost: 'rightmost' }) ],
    ),
  ];

pub []Expr addSubAdd = [ Text("+") ];
pub []Expr addSubSub = [ Text("-") ];
pub []Expr addSubAddMod = [ Text("+%") ];
pub []Expr addSubSubMod = [ Text("-%") ];

pub []Expr mulDivOp =
  [ Or(
      [ Match(powOp, 'left', { rightmost: ff }),
        space,
        Or(
          [ Match(mulDivMul, 'op') ],
          [ Match(mulDivDiv, 'op') ],
          [ Match(mulDivMulMod, 'op') ],
        ),
        space,
        Match(mulDivOp, 'rigt', { rightmost: 'rightmost' }),
      ],
      [ Match(powOp, 'expr', { rightmost: 'rightmost' }) ],
    ),
  ];

pub []Expr mulDivMul = [ Text("*") ];
pub []Expr mulDivDiv = [ Text("/") ];
pub []Expr mulDivMod = [ Text("*%") ];

pub []Expr powOp =
  [ Or(
      [ Match(unaryLeftOp, 'left', { rightmost: ff }),
        space,
        Text("**"),
        space,
        Match(unaryLeftOp, 'rigt', { rightmost: 'rightmost' }),
      ],
      [ Match(unaryLeftOp, 'expr') ],
      [ Match(true, 'rightmost'), Match(extraExpr, 'expr') ],
    ),
  ];

pub []Expr extraExpr =
  [ Or(
      [ Match(returnExpr, 'expr') ],
      [ Match(breakExpr, 'expr') ],
      [ Match(comptimeExpr, 'expr') ],
      [ Match(constExpr, 'expr') ],
      [ Match(assignOp, 'expr') ],
    ),
  ];

pub []Expr returnExpr =
  [ Text('return'),
    Match(namedControlFlow),
    Maybe(
      space,
      Match(expression, 'expr', { rightmost: tt })
    ),
  ];

pub []Expr breakExpr =
  [ Text('break'),
    Match(namedControlFlow),
    Maybe(
      space,
      Match(expression, 'expr', { rightmost: tt }),
    ),
  ];

pub []Expr namedControlFlow = [ Text("-"), Match(lIdentifier, 'name') ];

pub []Expr comptimeExpr =
  [ Text('comptime'),
    space,
    Match(expression, 'expr', { rightmost: tt }),
  ];

pub []Expr constExpr =
  [ Text('const'), space,
    Match(expression, 'expr', { rightmost: tt }),
  ];

pub []Expr assignOp =
  [ Match(unaryRightOp, 'left'),
    space,
    Or(
      [ Match(asgn, 'op') ],
      [ Match(asgnAdd, 'op') ],
      [ Match(asgnSub, 'op') ],
      [ Match(asgnMul, 'op') ],
      [ Match(asgnDiv, 'op') ],
      [ Match(asgnMod, 'op') ],
      [ Match(asgnPow, 'op') ],
    ),
    space,
    Match(expression, 'rigt', { rightmost: tt }),
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
        space,
        Match(unaryLeftOp, 'sub'),
      ],
      [ Match(unaryRightOp, 'sub') ],
    ),
  ];

pub []Expr unaryLeftAwait = [ Text('await') ];
pub []Expr unaryLeftAwait = [ Text('nowait') ];
pub []Expr unaryLeftAwait = [ Text('ignore') ];
pub []Expr unaryLeftAwait = [ Text("!") ];

// asdf

pub []Expr unaryRightOp =
  [ Or(
      [ Match(unaryRightOp, 'sub'),
        space,
        Or(
          [ Match(memberAccess, 'op'), Match(lIdentifier) ],
          [ Match(index, 'op'), Match(expression, 'index'), Text("]") ],
        ),
      ],
      [ Match(lIdentifier, 'lIdentifier') ],
      [ Match(type, 'type') ],
      [ Match(fnCall, 'fnCall') ],
      [ Match(blockExpr, 'blockExpr') ],
      [ Match(switchExpr, 'switchExpr') ],
      [ Match(forExpr, 'forExpr') ],
      [ Match(continueExpr, 'continueExpr') ],
      [ Match(numberLit, 'numberLit') ],
      [ Match(stringLit, 'stringLit') ],
      [ Match(varDef, 'varDef') ],
      [ Match(arrayLit, 'arrayLit') ],
      [ Match(tupleLit, 'tupleLit') ],
      [ Match(objectLit, 'objectLit') ],
      [ Match(setLit, 'setLit') ],
    ),
  ];

pub []Expr memberAccess = [ Text(".") ];
pub []Expr index = [ Text("[") ];

pub []Expr type = [ Text("[") ];

pub []Expr continueExpr = [ Text('continue'), Match(namedControlFlow) ];

// An definition that ends with a curly brace and must not end with a semicolon.
pub []Expr expressionCB =
  [ Or(
      [ Match(fnDef, 'expr') ],
    )
  ];

// Old code below
pub []Expr definitions =
  [ Repeat(
      [ Or(
          [ Match(objectDestructuring, 'definitions'
              { typed: Destructuring.Typed.never }
            )
          , Space('')
          , Text(";")
          ],
          [ Match(variableDefinition, 'definitions')
          , Space('')
          , Text(";")
          ],
          [ Match(classDefinition, 'definitions', ) ],
          [ Match(traitDefinition, 'definitions', ) ],
          [ Match(functionDefinition, 'definitions') ],
        )
      ],
      Space("\n"),
      Startspace,
      Space("\n"),
    )
  ];

pub []Expr variableDefinition =
    [ Match(type, 'type')
    , Space(" ")
    , Match(identifier, 'name')
    , Or(
        [ [ Space(" "), Text("="), MSpace(" ", 4, [ Match(expression, 'init') ]) ]
        , [ Equals('init', null) ]
        ]
      );
    ];
}

pub []Expr classDefinition =
    [ Text('class')
    , Space(" ")
    , Match(uIdentifier, 'name')
    , Maybe(
        [ Space('')
        , Text("<")
        , Space('')
        , Repeat(
            [ Match(parameter, 'params') ],
            [ Text(","), MSpace(" ", 6) ]
          )
        , Text(">")
        ]
      )
    , Maybe(
        [ Space(" ")
        , Text(":")
        , Space(" ")
        , Repeat([ Match(Identifier, 'extends') ], [ Text(","), Space("\n") ])
        ]
      )
    , Maybe(
        [ Space(" ")
        , Text('friend')
        , Space(" ")
        , Repeat([ Match(Identifier, 'friends') ], [ Text(","), Space("\n") ])
        ]
      )
    , Space(" ")
    , Text("{")
    , MSpace("\n", 2,
        [ Repeat(
            [ StartSpace()
            , Or(
                [ Match(classDefinition, 'members') ],
                [ Match(traitDefinition, 'members') ],
                [ Match(functionDefinition, 'members') ],
                [ Match(variableDefinition, 'members') ],
                [ Match(destructuringDefinition, 'members') ],
              )
            ],
            [ Space("\n"), Startspace, Space("\n") ],
          )
        ]
      )
    , Space("\n")
    , StartSpace()
    , Text("}"
    ];
}

pub []Expr traitDefinition =
    [ Text('trait')
    , Space(" ")
    , Match(uIdentifier, 'name')
    , Maybe(
        [ Space('')
        , Text("<")
        , Space('')
        , Repeat(
            [ Match(parameter, 'params') ],
            [ Text(","), MSpace(" ", 6) ]
          )
        , Text(">")
        ]
      )
    , Maybe(
        [ Space(" ")
        , Text(":")
        , Space(" ")
        , Repeat([ Match(Identifier, 'extends') ], [ Text(","), Space("\n") ])
        ]
      )
    , Maybe(
        [ Space(" ")
        , Text('friend')
        , Space(" ")
        , Repeat([ Match(Identifier, 'friends') ], [ Text(","), Space("\n") ])
        ]
      )
    , Space(" ")
    , Text("{")
    , MSpace("\n", 2,
        [ Repeat(
            [ StartSpace()
            , Or(
                [ Match(classDefinition, 'members') ],
                [ Match(traitDefinition, 'members') ],
                [ Match(functionDefinition, 'members') ],
                [ Match(functionDeclaration, 'declarations') ],
              )
            ],
            [ Space("\n"), Startspace, Space("\n") ],
          )
        ]
      )
    , Space("\n")
    , StartSpace()
    , Text("}"
    ];

pub []Expr functionSignature =
    [ Match(type, 'returnType')
    , Text("(")
    , Releat(
        [ Match(parameter, 'params') ],
        [ Text(","), Space(" ") ],
      )
    , Text(")")
    ];

pub []Expr functionDefinition =
    [ Match(functionSignature)
    , Or(
        [ Text("=>"), Match(expression, 'body') ],
        [ Match(block, 'body') ],
      )
    ];

pub []Expr functionDeclaration =
    [ Match(functionSignature), Space(''), Text(";") ];

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
                  [ [ Text('class') ], [ Text('trait') ], [ Text('type') ] ]
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
