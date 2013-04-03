/* 
Copyright 2013 Google Inc.

Licensed under the Apache License, Version 2.0 (the ''License''); 
you may not use this file except in compliance with the License.  
You may obtain a copy of the License at  
http://www.apache.org/licenses/LICENSE-2.0 
*/
library test_combinatorial_parsing;

import 'CombinatorialParsing.dart';
import 'smalltalkParser.dart';
import 'package:unittest/unittest.dart';

class Test1 extends ExecutableGrammar {
 CombinatorialParser abc, de, start, alt;
 Test1() {
   abc = char('a') & char('b') & char('c');
   de = char('d') & char('e').plus;
   alt = abc | de;
   start = whitespace.opt & (abc | de);
 }
}

class Test2 extends ExecutableGrammar {
 CombinatorialParser digit, number;
 Test2() {
   digit = charBetween("0", "9");
   number = digit.plus;//number & digit | digit;
 }
}

main() {
  var x;
 Test1 t1 = new Test1();
 print(x = t1.start.parse("abc"));
 print (x = t1.whitespace.opt.parse("   "));
 print(x = t1.start.parse("deeee"));
 try {print(t1.start.parse("d"));} on ParserError catch(p){print(p);};

 Test2 t2 = new Test2();
 print(t2.number.parse("0"));
 print(t2.number.parse("1234"));
 
 //SmalltalkGrammar t3 = new SmalltalkGrammar();
 SmalltalkParser t3 = new SmalltalkParser();
 print(x = t3.identifier.parse('b1234'));
 print(x = t3.expression.parse('foo := self bar: x bam baz *2 + 4'));
 print(x = t3.method.parse('''forEach: x 
foo := self bar: x bam baz *2 + 4.
[:x | x**2] value: 3.
^#shazam'''));
}