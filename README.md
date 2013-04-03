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

RunnableGrammar was introduced because ongoing changes to the Dart platform invalidated the code in ExecutableGrammar in 
a way that was difficult to correct. Specifically, changes in the behavior of futures made it difficult to use mirrors 
within a single isolate in the manner they were designed for. We expect forthcoming changes to dart:mirrors to address the
problem in the near future. However, RunnableGrammar may still be advantageous is some ways.

The current DartGrammar class inherits from ExecutableGrammar, but as I say, this is trivial to change. 
More difficult is debugging the grammar so it is truly accurate. 
There are examples of functioning Smalltalk grammars and parsers, in versions that use the either ExecutableGrammer 
(smalltalkGrammar.dart, smalltalkParser.dart) and RunnableGrammar (smalltalkGrammarRevised.dart, smalltalkRevisedParser.dart). 
The latter two might actually work. And there is a little bit of test code in testParserCombinators.dart.

One advantage of RunnableGrammar is that it is no longer necessary to declare all productions as fields of the grammar class. 
In the original Newspeak design, each production was declared and initialized as a slot in the classes' instance initializer.
However, in Dart this solution did not work quite so well. Field initializers in Dart are not allowed to access 'this' and
since almost any production requires such access, the pattern was to declare the fields, and then initialize them inside
the constructor. This duplication is annoying to write and harder to read.

Since RunnableGrammar maintains the actual production objects in a map, no fields need to be declared.  Instead, references to
productions in the constructor call non-existant setters and getters which are trapped by noSuchMethod and handled properly.
This does mean that actual parsing is likely to be slower, as each use of a production goes through noSuchMethod. We have not
yet established the nature of the performance differences.

Another issue is thye abundance of spurious warnings one gets using RunnableGrammar, as all productions are undefined from 
the perspective of the Dart analyzer. Planned silencing mechanisms like @ExpectWarnings will help.
