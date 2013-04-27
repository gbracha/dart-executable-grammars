/* 
   Copyright 2008 Cadence Design Systems, Inc.
   Copyright 2013 Google Inc.
   
   Licensed under the Apache License, Version 2.0 (the ''License''); 
   you may not use this file except in compliance with the License.  
   You may obtain a copy of the License at  http://www.apache.org/licenses/LICENSE-2.0
*/

library smalltalk_grammar;
import 'CombinatorialParsing.dart';
import 'UnarySelectorParser.dart';

class SmalltalkGrammar extends ExecutableGrammar{
  CombinatorialParser colon, comma, dollar, dot, equalSign, hat, lbracket,
    lcurly, lparen, langlebracket, pound, ranglebracket, rbracket, rcurly, 
    rparen, semicolon, slash, vbar, digit, digits, extendedDigits, radix,
    fraction, extendedFraction, exponent, decimalNum, radixNum, num, number,
    letter, specialCharacter, character, id, identifier, Char, 
    characterConstant, twoQuotes, str, string, kw, kws, keyword, sym, symbol,
    commentDelimiter, beginComment, endComment, twoDblQuotes, comment, binSel,
    binarySelector, assign, assignment, symbolConstant, array, arrayConstant,
    tuple, literal, variableName, unarySelector, parenthesizedExpression,
    primary, unaryExpression, binaryMsg, binaryExpression, keywordMsg, keywordExpression,
    cascadeMsg, cascadedMessageExpression, assignmentLHS, expression,
    returnStatement, furtherStatements, statementSequence, statements,
    blockParameter, blockParameters, varDecls, temporaries, codeBody, block,
    variableDecl, unaryMsgPattern, binaryMsgPattern, keywordMsgPattern,
    messagePattern, pragma, methodBody, method, methodDecl, category, 
    classComment, classBody, classSideDecl, languageId, classCategory,
    classFormat, classDefinition, extendedClass, packageName, package;
  
  SmalltalkGrammar() {
    // tokens
    
    colon = tokenFromChar(':');
    comma = tokenFromChar(',');
    dollar = tokenFromChar('\$');  
    dot = tokenFromChar('.');
    equalSign = tokenFromChar('=');
    hat = tokenFromChar('^');
    lbracket = tokenFromChar('[');
    lcurly = tokenFromChar('{');  
    lparen = tokenFromChar('(');
    langlebracket = tokenFromChar('<');
    pound = tokenFromChar('#');
    ranglebracket = tokenFromChar('>');
    rbracket = tokenFromChar(']');  
    rcurly = tokenFromChar('}');
    rparen = tokenFromChar(')');
    semicolon = tokenFromChar(';');
    slash = tokenFromChar('//');
    vbar = tokenFromChar('|');  

    // lexical grammar
  
    digit = charBetween('0', '9');
    digits = digit.plus;  
    extendedDigits = (digit | letter).plus;
    radix = (digits & char('r')).wrapper(
          (ds, r) => ds
        );
    fraction = (dot & digits).wrapper(
          (period, ds) => ds
        );
    extendedFraction = (extendedDigits & char('r')).wrapper(
        (ds, r) => ds
      );
    exponent = (char('e') & char('-').opt & digits).wrapper(
        (e, sgn, ds) => sgn == null ? ds : ds //?
 );
decimalNum = char('-').opt & digits & fraction.opt & exponent.opt;
radixNum = radix & char('-').opt & extendedDigits & extendedFraction.opt & exponent.opt;
num = radixNum | decimalNum;

/* must distinguish internal use of productions from use as tokens */ 
number = tokenFor(num);
letter = new PredicateTokenParser((c) => isLetter(c), 'letter expected');

specialCharacter =
char('+') | char('/') |  
char('\\') | char('*') |
char('~') | char('<') | char('>') |
char('=') | char('@') |
char('%') | char('|') |
char('&') | char('?') |
char('!') | char(',') | char('`');

character = digit | letter | specialCharacter | //?
char('[') | char(']') |
char('{') | char('}') | char('(') |
char(')') | char('^') | char(';') | char('\$') | char('#') |
char(':') | char('.') | char('-') | char('_') | char('`') 
/* the Smalltalk grammar neglects to add - to characters, or to comments. 
 * It does add | [char: $' ], but these are both bugs. 
 * We intend to support underscores, which Squeak insists on turning into assignment arrows. 
 * However, we do not support these as assignments. 
 * At the moment, we do not accept them in identifiers, but this will change in time. 
 * */;

id = (letter & (letter | digit | char('_')).star).wrapper(
        (fst, snd) => '$fst${new StringBuffer()..writeAll(snd)}'
        );

identifier = tokenFor(id);

Char = 
  char('\$') & new PredicateTokenParser(()=> true, "");

characterConstant = tokenFor(Char);

twoQuotes = (char("'") & char("'")).wrapper((q1, q2) => "'");

str = (char("'") & stringBody & char("'")).wrapper(
    (oq, es, eq) => es.inject('', (s, e) => '$s$e')
    );

string = tokenFor(str);

kw = (id & char(':')).wrapper((i, c) => '$i:');

kws = kw.plus.wrap((c) => c.inject('', (s, e) => '$s$e'));  

keyword = tokenFor(kw);

sym = str | kws | binSel | id;

symbol = tokenFor(sym);

commentDelimiter = char('"');

beginComment = commentDelimiter;

endComment = commentDelimiter;

twoDblQuotes = (char('"') & char('"')).wrapper((q1, q2) => '"');

comment = beginComment & commentBody & endComment;

binSel = ((specialCharacter | char('-')) & specialCharacter.opt & specialCharacter.opt).wrapper(
      (c1, c2, c3) => '${c1 == null? "": c1}${c2 == null? "": c2}${c3 == null? "": c3}'); //asSymbol?

binarySelector = tokenFor(binSel);    

assign = (char(':') & char('=')).wrapper((c, e) => ':=');

assignment = tokenFor(assign | char('_')); /* Temporary hack */

/* syntacticGrammar */

symbolConstant = pound & symbol;

array = (lparen & (number | symbolConstant | symbol | string | characterConstant | arrayConstant | array).star & rparen)
|
/* Byte array literal */
(lbracket & number.star & rbracket);

arrayConstant = pound & array;

tuple = lcurly & expression.starSeparatedOrTerminatedBy(dot) & rcurly;

literal = number | symbolConstant | characterConstant | string | arrayConstant | tuple;

variableName = identifier;

unarySelector = new UnarySelectorParser(this); /* the one hack/flaw. See UnarySelector parser for details */

parenthesizedExpression = lparen & expression & rparen;

primary = variableName | literal | block | parenthesizedExpression;

unaryExpression = primary & unarySelector.star;

binaryMsg = binarySelector & unaryExpression;

binaryExpression = unaryExpression & binaryMsg.star;

keywordMsg = (keyword & binaryExpression).plus;

keywordExpression = binaryExpression & keywordMsg.opt;

cascadeMsg = semicolon & (keywordMsg | binaryMsg | unarySelector);

cascadedMessageExpression = keywordExpression & cascadeMsg.star;

assignmentLHS = variableName & assignment;

expression = assignmentLHS.star & cascadedMessageExpression;

returnStatement = hat & expression & dot.opt;

furtherStatements = dot & statements;

statementSequence = (expression & furtherStatements.opt) | furtherStatements;

statements = returnStatement | statementSequence | empty;

blockParameter = colon & variableDecl;

blockParameters = blockParameter.plus & vbar;

varDecls = vbar & variableDecl.star & vbar;

temporaries = varDecls;

codeBody = temporaries.opt & pragma.star & statements;

block = (lbracket & blockParameters.opt & codeBody & rbracket) | (lbracket & blockParameter.star & rbracket);

variableDecl = identifier;

unaryMsgPattern = unarySelector;

binaryMsgPattern = binarySelector & variableDecl;

keywordMsgPattern = (keyword & variableDecl).plus;

messagePattern = unaryMsgPattern | binaryMsgPattern | keywordMsgPattern;

/* Yuck, a pragma can appear before or after |temps| */
pragma = langlebracket & (unarySelector | (keyword & (literal | variableDecl)).plus) & ranglebracket;

methodBody = pragma.star & codeBody;

method = messagePattern & methodBody & eoi; /* A method in a browser */

/* Top level productions for classes */

methodDecl = messagePattern & equalSign & lparen & methodBody & rparen;

category = string & methodDecl.star;

classComment = whitespace & comment; /* A hack, to preserve comments from a complete class declaration */

classBody = lparen & classComment.opt & varDecls.opt & varDecls.opt & varDecls.opt & category.star & rparen;

classSideDecl = colon & classBody;

languageId = identifier;

classCategory = string /*.opt */;

classFormat = 
     tokenFromSymbol('class') | 
     tokenFromSymbol('weakclass') | 
     tokenFromSymbol('variableclass') | 
     tokenFromSymbol('variablebyteclass') | 
     tokenFromSymbol('variablewordclass');

classDefinition = classCategory & classFormat & identifier & equalSign & identifier & classBody & classSideDecl.opt;

extendedClass = tokenFromSymbol('extensions') & identifier & equalSign & lparen & category.star & rparen & colon & lparen & category.star & rparen;

packageName =  string;

package = languageId & tokenFromSymbol('package') & packageName & equalSign & lparen & classDefinition.star & extendedClass.star & rparen & eoi;
}

// as yet unclassified
CombinatorialParser get commentBody {

/* As an optimization, we process the body of a comment with a dedicated scanning parser. 
It should be equivalent to:

return (character | aWhitespaceChar | char("'")  | twoDblQuotes).star */
return new CollectingCommentParser( (input) {
      var c = input.peek;
      if (c == null) false; /* let main routine handle end of input */
      else if (c == '"') false; 
      else {
        var pos = input.position;
        input.next;
        if (input.peek == '"') false; else {input.position = pos; true;}
      }});
}


CombinatorialParser get stringBody {

/* As an optimization, we process the body of a string with a dedicated scanning parser. 
It should be equivalent to:

return (character | aWhitespaceChar | char('"')  | twoDblQuotes).star */
return new CollectingCommentParser((input) {
      var c = input.peek;
      if (c == null) false; /* let main routine handle end of input */
      else if (c == "'") false; 
      else {
        var pos = input.position;
        input.next;
        if (input.peek == "'") false; else {input.position = pos; true;}
      }});
  }

bool isLetter(c) {
  int
  code = c.codeUnitAt(0);
  return (code >= 97 && code <= 122) || (code >= 65 && code <= 90);
}
}

