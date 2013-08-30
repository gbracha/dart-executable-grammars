/* 
Copyright 2013 Google Inc.

Licensed under the Apache License, Version 2.0 (the ''License''); 
you may not use this file except in compliance with the License.  
You may obtain a copy of the License at  
http://www.apache.org/licenses/LICENSE-2.0 
*/
library test_dart_grammar;

import 'CombinatorialParsing.dart';
import 'dart_runnable_grammar.dart' as dartRunnableGrammar;
import 'dart_executable_grammar.dart' as dartExecutableGrammar;

accept(production, input) {
  try {
  	production.parse(input);
  	// print("PASS: Accepted <$input>");
  } catch(e) {
    print("FAIL: Should have accepted <$input>");  	
  }
}

reject(production, input) {
  try {
  	production.parse(input);
  	throw "FAIL: Should have rejected <$input>";
  } catch(e) {
  	// print("PASS: Rejected <$input>");
  }
}

test(grammar) {
  accept(grammar.identifier, 'foo');
  accept(grammar.identifier, 'bar9');
  accept(grammar.identifier, 'dollar\$');
  accept(grammar.identifier, '_foo');
  accept(grammar.identifier, '_bar9');
  accept(grammar.identifier, '_dollar\$');
  accept(grammar.identifier, '\$');
  accept(grammar.identifier, ' leadingSpace');
  reject(grammar.identifier, '9');
  reject(grammar.identifier, '3foo');
  reject(grammar.identifier, '');

  accept(grammar.numericLiteral, '0');
  accept(grammar.numericLiteral, '1984');
  accept(grammar.numericLiteral, ' 1984');
  accept(grammar.numericLiteral, '-1984');
  accept(grammar.numericLiteral, '0xCAFE');
  accept(grammar.numericLiteral, '0XCAFE');
  accept(grammar.numericLiteral, '0xcafe');
  accept(grammar.numericLiteral, '0Xcafe');
  accept(grammar.numericLiteral, '0xCaFe');
  accept(grammar.numericLiteral, '0XCaFe');
  accept(grammar.numericLiteral, '3e4');
  accept(grammar.numericLiteral, '3e-4');
  accept(grammar.numericLiteral, '-3e4');
  accept(grammar.numericLiteral, '-3e-4');
  accept(grammar.numericLiteral, '3E4');
  accept(grammar.numericLiteral, '3E-4');
  accept(grammar.numericLiteral, '-3E4');
  accept(grammar.numericLiteral, '-3E-4');
  reject(grammar.numericLiteral, '-3e--4');
  reject(grammar.numericLiteral, '-0xCAFE');
  reject(grammar.numericLiteral, '-0XCAFE');
  reject(grammar.numericLiteral, 'CAFE');
  reject(grammar.numericLiteral, '0xGHIJ');
  reject(grammar.numericLiteral, '-');
  reject(grammar.numericLiteral, '');

  accept(grammar.booleanLiteral, 'true');
  accept(grammar.booleanLiteral, 'false');
  accept(grammar.booleanLiteral, ' true');
  accept(grammar.booleanLiteral, ' false');
  reject(grammar.booleanLiteral, '9');
  reject(grammar.booleanLiteral, '"foo"');
  reject(grammar.booleanLiteral, "'foo'");
  reject(grammar.booleanLiteral, 'TRUE');
  reject(grammar.booleanLiteral, 'FALSE');
  reject(grammar.booleanLiteral, 'null');
  reject(grammar.booleanLiteral, '0xCAFE');

  // Do we want to use the consuming or non-consuming variant of #not?
  //accept(grammar.stringLiteral, '"foo"');
  //accept(grammar.stringLiteral, "'foo'");
}

main() {
  print('Runnable:');
  test(new dartRunnableGrammar.DartGrammar());
  print('Executable:');
  test(new dartExecutableGrammar.DartGrammar());
}
