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
import 'dartGrammar.dart' as dartExecutableGrammar;

accept(production, input) {
  try {
  	production.parse(input);
  	print("PASS: Accepted <$input>");
  } catch(e) {
    print("FAIL: Should have accepted <$input>");  	
  }
}

reject(production, input) {
  try {
  	print(production.parse(input));
  	throw "FAIL: Should have rejected <$input>";
  } catch(e) {
  	print("PASS: Rejected <$input>");
  }
}

test(grammar) {
  reject(grammar.identifier, '9');
  reject(grammar.identifier, '');
  accept(grammar.identifier, 'foo');
  accept(grammar.identifier, 'bar9');
  accept(grammar.identifier, 'dollar\$');
  accept(grammar.identifier, '_foo');
  accept(grammar.identifier, '_bar9');
  accept(grammar.identifier, '_dollar\$');
  accept(grammar.identifier, '\$');
  accept(grammar.identifier, ' leadingSpace');
}

main() {
  print('runnable');
  test(new dartRunnableGrammar.DartGrammar());
  print('executable');
  test(new dartExecutableGrammar.DartGrammar());
}
