dart-executable-grammars
========================

A parser combinator library for Dart based on a port of Newspeak's executable grammars. See

http://bracha.org/executableGrammars.pdf

for a discussion of the main ideas.

The original Newspeak implementation relied on reflection as described in the above paper, and in the slides at

http://bracha.org/newspeak-parsers.pdf 


The class ExecutableGrammar embodies that logic. The class RunnableGrammar embodies a variation that uses noSuchMethod instead. 
All one needs to do to switch from one implementation to the other is to inherit from RunnableGrammar instead of 
ExecutableGrammar.

RunnableGrammar was introduced because ongoing changes to the Dart platform invalidated the code in ExecuatbleGrammar in 
a way that was difficult to correct. Specifically, changes in the behavior of futures made it difficult to use mirrors 
within a single isolate in the manner they were designed for. We expect forthcoming changes to dart:mirrors to address the
problem in the near future. However, RunnableGrammar may still be advantageous is some ways.

The current DartGrammar class inherits from ExecutableGrammar, but as Isay, this is trivial to change. 
More difficult is debugging the grammar so it is truly accurate. 
There are examples of functioning Smalltalk grammars and parsers, in versions that use the either ExecutableGrammer 
(smalltalkGrammar.dart, smalltalkParser.dart) and RunnableGrammar (smalltalkGrammarRevised.dart, smalltalkParserRevised.dart). 
The latter two might actually work. And there is a little bit of test code in testParserCombinators.dart.
