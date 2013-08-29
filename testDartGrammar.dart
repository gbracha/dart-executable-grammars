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
  print("Accepted ${production.parse(input)}");
}

reject(production, input) {
  try {
  	print(production.parse(input));
  	throw "Should have rejected <$input>";
  } catch(e) {
  	return;
  }
}

test(grammar) {
  accept(grammar.identifier, 'a');
  reject(grammar.identifier, '9');
  reject(grammar.identifier, '');

  
}

main() {
  print('runnable');
  test(new dartRunnableGrammar.DartGrammar());
  print('executable');
  test(new dartExecutableGrammar.DartGrammar());
}
