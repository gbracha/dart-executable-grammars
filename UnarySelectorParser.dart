/* 
   Copyright 2008 Cadence Design Systems, Inc.
   Copyright 2013 Google Inc.
   
   Licensed under the Apache License, Version 2.0 (the ''License''); 
   you may not use this file except in compliance with the License.  
   You may obtain a copy of the License at  http://www.apache.org/licenses/LICENSE-2.0
*/
library unary_selector_parser;
import 'CombinatorialParsing.dart';


class UnarySelectorParser extends CombinatorialParser {
/* In the absence of a separate lexer, an ambiguity arises, which this parser deals with.
 The problem is that when parsing a unary expression that is an argument to a keyword
 message, one expects a unary selector, which is an identifer.  However, it may be that the next
 token is actually a keyword, part of the surrounding message. If we aren't actually tokenizing,
 the prefix of the keyword will be misinterpreted as an identifier and hence as another unary
 message being sent to the argument.
 
 Using a lexer solves this but introduces a subtlety around the assignment operator :=,. In that case
 if there is no whitespace between a variable name and the assignment, the variable name will
 be tokenized as a keyword rather than as an identifier. The Strongtalk parser, DeltaParser, deals
 with this specially. In the longterm, that is probably the way to go.

*/
	var p;

UnarySelectorParser(this.p);

parseWithContext(context) {
  var pos = context.position;
  try {
    p.keyword.parseWithContext(context);
  } catch (e) {  
		context.position = pos;
		return p.identifier.parseWithContext(context);
    };
  context.position = pos;
  throw new ParserError('should not print', pos);
}

}