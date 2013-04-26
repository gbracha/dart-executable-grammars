library combinatorialParsing;

/* A Dart version of the Newspeak parser combinator library.

Copyright 2008 Cadence Design Systems, Inc.
Copyright 2012 Cadence Design Systems, Inc.
Copyright 2013 Google Inc.

Licensed under the Apache License, Version 2.0 (the ''License''); 
you may not use this file except in compliance with the License.  
You may obtain a copy of the License at  
http://www.apache.org/licenses/LICENSE-2.0 
*/

  import 'dart:mirrors';
  import 'dart:async';

class AlternatingParser extends CombinatorialParser {
/* A parser that parses either P or Q. */

   CombinatorialParser p;
   CombinatorialParser q;

   AlternatingParser(this.p, this.q);

// parsing
  parseWithContext(ParserContext context) {
    var pos = context.position;
    try {
    	return  p.parseWithContext(context);
    } on ParserError catch (e1) {
        context.position = pos;
        context.recordFailure([e1.message, e1.position]);
        try {
          return q.parseWithContext(context);
        } on ParserError catch(e2) {
          context.recordFailure([e2.message, e2.position]);
          if (e1.position > e2.position) throw e1;
          if (e2.position > e1.position) throw e2;
          _combineErrorsWith(e1.message, e2.message, e1.position);
      }}
  }

// private
   _combineErrorsWith(String e1, String e2, int pos) {
	String msg;
	String or = ((e1 == '') || (e2 == '')) ? '' :'or';
	if (e1 == e2) {msg = e1;} else msg = '$e1 $or $e2';
	return throw new ParserError(msg, pos);
	}
}

class CharParser extends  PredicateTokenParser {
   /* This constructor leverages the assumption that characters are immutable values. Hence, if an input
   equals the specified token, it is indistiguishable from it, and we can just return token as the result 
   of the parse. Consequently, the wrapper function we pass to the superclass constructor ignores its 
   input and returns token. */
   CharParser(String tokenToParse): super((t){ return t == tokenToParse;}, '${tokenToParse} expected');  
}


class CollectingCommentParser extends CommentParser {
/* A special parser used for inputs that need to be rapidly scanned over. 
It differs from its superclass in that it actually collects the characters it scans, in case they are needed (e.g., for pretty printers). 

Ideally,we should not have to do this, but until we do proper optimization by compiling combinators, this will have to suffice. 
It provides a marked improvement in performance, 
By using such parsers for comments, whitespace and strings, the overall performance of the Newspeak parser improved by a factor of 2 or so. 
*/

    var comment;

    CollectingCommentParser(tb): super(tb);
// parsing
     parseWithContext(ParserContext context) {
      var c;

      comment = new List();
      while (!termBlock(context)) { 
	       c = context.nextIfAbsent(
	             () => throw new ParserError('Premature end of input', context.position-1)
	           );
         comment.add(c);
      }; 
  	  return  comment;
    }
}

abstract class CombinatorialParser {
/* This class is intended to implement Parser
Combinators. A CombinatorialParser<T>
returns a value of type T after successful
parsing.

The class is abstract. It does not implement
 the parsing routine parseWithContext() .

Concrete subclasses should implement specific grammars.

Parsing is initiated by calling parse(). This routine takes a String as input.
If parsing fails, it is the caller's responsibility to set the input stream back to its original position
(Q: is this a good idea?).
 */

   String name; 

/* Used to bypass 0 .. n ForwardReferenceParsers to get to the real parser.  Usually, this is 'this'. Only ForwardReferenceParsers forward the request to their forwardee */
get ultimateParser => this;

recordFailure(f) => this; /* Do nothing, save time */ //?
	
get value => this; //?

// combinators

	CombinatorialParser operator << (CombinatorialParser p) {
 	/* The sequencing combinator (implicit in BNF). */
		var o = new List()..add(this)..add(p);
		return new SequentialParser(o);
  }

  SequentialParser operator & (CombinatorialParser p) {
       /* The flattening sequencing combinator. This is what one should typically use in a grammar.
        * It differs from '<<' in its specification.  '<<' is not intended to flatten the resulting parser tree, 
        * while '&' is; this achieved by overriding '&' in SequentialParser to do the flattening.
        * 
        * 	Why would one want to flatten the tree? Because, given a production
        * 	
        * 		Foo -> Bam Ban Bar Bat
        * 		
        * 		one doesn't want to build the AST by writing
        * 		
        * 			Foo = (Bam << Ban << Bar << Bat).wrapper((start, end) {
        * 			          FooNode.b1(start [0], start[1], start[2], end)
        * 			          }
        * 			          
        * It is much more convenient to flatten the tree and have a flat list of the correct arity.  However, we only 
        * want to flatten one level. Otherwise, we have an associative sequencing operator and we can no longer 
        * tell the arity of the sequence produced in a given production.
		    **/

		   return this << p;
  }



	CombinatorialParser get empty => new EmptyParser();

	CombinatorialParser get eoi => tokenFor(new EOIParser());

	CombinatorialParser get fail => new FailingParser();

	CombinatorialParser get not => new NegatingParser(this);

	/* [P] = P | e */
	CombinatorialParser get opt =>  this | empty;

	/* Return a parser that accepts one or more repetitions of what the receiver accepts. Denoted by the postfix + in BNF */
	/* P+ = P & P* ; However, we must flatten the list one level */
	CombinatorialParser get plus => new PlusParser(this);
	

	CombinatorialParser plusSeparatedBy(CombinatorialParser separator) {
	/* Utility for the common case of a list with separators. The separators are discarded, as they are usually only used to 
	 * guide parsing and have no semantic value.  If one needs them, one can always build the rule directly 
	 * */

	return (this <<  ((separator  << this).wrapper((s, v) =>  v)).star).
               wrapper((fst, rst){
                              new List()..add(fst)..addAll(rst);  /* could be optimized to reuse rst */
                           });
	}

	CombinatorialParser plusSeparatedOrTerminatedBy(CombinatorialParser separator) {
 
	/* Utility for the common case of a list with separators, allowing for an optional appearance of the separator at the end. 
	 * The separators are discarded, as they are usually only used to guide parsing and have no semantic value.  If one needs them, 
	 * one can always build the rule directly 
	 * */
	     return ( plusSeparatedBy(separator) &  separator.opt).wrapper((lst, end) => lst);
	}

 	/* Return a parser that accepts zero or more repetitions of what the receiver accepts. Denoted by the postfix * in BNF */
	/* P* = [P+] */
	/* We tweak the classic formulation by wrapping it in a parser that takes care to avoid returning null.
	In the ordinary case, if the input is empty, the empty parser will return null as the result. 
	However, we'd rather not  have to check for null every time we get a result from a starred
	production; it is verbose and error prone. In the case of star, it is better to return an empty list
	for empty input. The call to wrap: below accomplishes that. */
	/* would be good to cache this, as well as plus and opt */
	CombinatorialParser get star => new StarParser(this);

	CombinatorialParser starSeparatedBy(CombinatorialParser separator) {
	   /* See analogous plus methods. Must wrap to prevent returning null in empty case */
	   	   return plusSeparatedBy(separator).opt.wrap((rs) => rs == null ? new List() :rs);
	}

	CombinatorialParser starSeparatedOrTerminatedBy(CombinatorialParser separator) {

	/* See analogous plus methods. Must wrap to prevent returning null in empty case */
       		 return plusSeparatedOrTerminatedBy(separator).opt.wrap((rs) => rs == null ? new List() :rs);
	}

	/* The alternation combinator - denoted by | in BNF */
	CombinatorialParser operator | (CombinatorialParser p) => new AlternatingParser(this, p); 

// parsing
	parse(String input) => parseNoContext(input);

	parseNoContext(String input) {
	/* YK - a context-less protocol for speeding up parsing */
	/* Turns out maintaining a context is expensive in runtime and doesn't
	do much for locating errors. Experimenting with other error localization
	mechanism. To minimize impact, the parse:inContext:ifError: protocol
	is maintained, and a bogus reportFailure is implemented on this */ 
   	   return parseWithContext(new DummyParserContext(input));
	}

	parseWithContext(ParserContext context);

// private
   _combineErrors(e1, e2, pos) {
      String or = ((e1 == '') || (e2 == '')) ? '' :'or';
      String msg = e1 == e2 ? e1 : '$e1 $or $e2';
      throw new ParserError(msg, pos);
   }

// utilities
	CombinatorialParser get aWhitespaceChar {
		return new PredicateTokenParser((String c) => c.codeUnitAt(0) <=  ' '.codeUnitAt(0),
           	       	   			    'whitespace expected');
        }

	CombinatorialParser char(String c) {
 	      return new CharParser(c);
	}

	CombinatorialParser charBetween(String c1, String c2) {
	      	return new PredicateTokenParser(
	      	    (String c) => 
	      	        (c1.codeUnitAt(0) <= c.codeUnitAt(0)) && 
	      	        (c.codeUnitAt(0) <= c2.codeUnitAt(0)),
		         'character between  $c1 and $c2 expected');
        }

	CombinatorialParser get comment => fail;


	CombinatorialParser tokenFor(CombinatorialParser p) {

	/* Tokenizing involves throwing away leading whitespace and comments.
	In addition, it involves associating the token with a starting position within the input stream;
	We do the latter first by wrapping p in a TokenizingParser; then we prefix it with a parser
	that deals with whitespace and comments, and return the result. */

  	 var posParser = new TokenizingParser(p);

	  return ((whitespace | comment).star &  posParser).wrapper((dontCare, t) => t);
        /* type safety note: wrapper is only defined on SequentialParser. The call is always<
           statically unsafe but checked dynamically   (see its definition). One could 
           cast to a SequentialParser, but that would not be enough to silence
           the typechecker anyway 
           */
	/* Design note: It seems tempting to define a combinator, 'token', that returns a tokenized version of its receiver.
	 *   Alas, this doesn't  work out, since tokenization relies on concepts of whitespace and comment, which are often specific 
	 *   to a given grammar. Hence, the combinator needs to be an operation of the grammar, not of a specific production. 
	 **/
        }

  CombinatorialParser tokenFromChar(String c) =>  tokenFor(char(c));
	

	CombinatorialParser tokenFromSymbol(String s) => tokenFor(new SymbolicTokenParser(s));
	
	CombinatorialParser token(String s) => tokenFor(new SymbolicTokenParser(s));// we can probably eliminate the two variants above

 	/* It's rare that anyone will need to change this definition */
	 /* return  aWhitespaceChar plus. */
	 /* As an optimization, we process whitespace with a dedicated scanning parser. Of course, this regrettable,  
	 but it is a significant win. */
	CombinatorialParser get whitespace => new WhitespaceParser();

	wrap(blk) =>  new WrappingParser(this, blk);

	namedWrap(blk, msg) => new NamedWrappingParser(this,blk)..name = msg;


	wrapper(blk) {//? varying arity stuff here
	 	 return  wrap( (rs) => Function.apply(blk, (rs is Collection) ? rs : [rs])); //? can this cope with a singleton?
			/* return wrap: blk */
	}

	namedWrapper(blk, msg) {
		 return  namedWrap( (rs) => Function.apply(blk, (rs is Collection) ? rs : [rs]), msg);
	}

}

class CommentParser extends CombinatorialParser {
      var termBlock;
  CommentParser(this.termBlock);
// parsing
   parseWithContext(ParserContext context) {
  	 while (!termBlock(context)){
  		context.nextIfAbsent(() => throw new ParserError('Premature end of input', context.position-1));
  	 }; 
	 }
}

class EOIParser extends CombinatorialParser {
/* A parser that only succeeds at the end of the input. This addresses a common problem with combinator parsers. 
 * If there is garbage at the end of the input, no production matches it. 
 * Consequently, the parsers backtrack to the point where the legal input was consumed, without giving an error message 
 * about the junk at the end. 
 * */
  
// parsing
   parseWithContext(ParserContext context) {
      return context.atEnd ? true : throw new ParserError('Unexpected input', context.position+1);
   }
}


class EmptyParser extends CombinatorialParser {
/* The parser that parses the empty input. It always succeeds. This class is a singleton. */
// parsing
   parseWithContext(ParserContext context) => null;
}

class FailingParser extends CombinatorialParser {
/* The parser that always fails. It never parses anything. This class is a singleton. */
// parsing
   parseWithContext(ParserContext context) =>  
       throw new ParserError('Failing Parser invoked', context.position);
}


class NamedWrappingParser extends WrappingParser {
/* This is exactly the same as a WrappingParser, but it passes itself down 
 * in the context parameter, to provide more meaningful error messages. 
 * */

   NamedWrappingParser(CombinatorialParser p, Function b): super(p, b);

   parseWithContext(ParserContext context) => wrapperBlock(parser.parseWithContext(context));
}


class NegatingParser extends CombinatorialParser {
/* A parser that implements the  'not' combinator, as in Ford's PEGs. 
 * It contains a parser p, and succeeds if p fails and vice versa. 
 * It does not move the input forward if it succeeds. 
 * */
   CombinatorialParser p;
   NegatingParser(this.p);

// parsing

   parseWithContext(ParserContext context) {
     var position = context.position;
     try {
       p.parseWithContext(context);
     } on ParserError catch (e) {
        context.position = position; 
        return true;
     };
     throw new ParserError('not combinator failed', position);
   }
}

abstract class ParserContext {
/* This class defines a context that is shared among a set of combinatorial 
 * parsers during a parse.  
 * Information includes the position of the input and could also include
 *  error tracking information, memoization, state for context-sensitive 
 *  grammars etc.
*/
  final List failures  = new List(); 
  final String _input;
  int position = 0;

  ParserContext(this._input); 
  
  bool get atEnd => position >= _input.length;
  String get next => atEnd? null : _input[position++];
  String  nextIfAbsent(Function handler) 
    => atEnd? handler() : _input[position++];
  String  peekIfAbsent(Function handler) => atEnd? handler() : _input[position];

  recordFailure(f) ;
}

class ErrorTrackingParserContext extends ParserContext {
/* This class defines a context that is shared among a set of combinatorial 
 * parsers during a parse. The context can be used to manage information on 
 * parsing errors: rather than always report the latest failure that occurred, 
 * we can report the one that occurred deepest in the input stream, or 
 * implement some other policy - as long as we can record what failures took 
 * place.
*/
	final List failures  = new List(); 

	ErrorTrackingParserContext(String input): super(input);  
   get errorMessage {
   	if (failures.isEmpty) return '';
	  return failures.last.first;//? protocol
   }

   get errorPosition {
   	if (failures.isEmpty) return -1;
	return failures.last.last;
   }

   recordFailure(f) {
   		   if (failures.isEmpty || (failures.last.last <= f.last)) failures.add(f);
   }
}

class DummyParserContext extends ParserContext {
/* A dummy context to save on the cost of error recording */
  
   DummyParserContext(String input): super(input); 
   get errorMessage => '';
   get errorPosition => -1;
   recordFailure(f) {} // do nothing
}

class ParserError extends Error {  
      String message;
      int position;
      
      ParserError(this.message, this.position);
// as yet unclassified
      get description => 'ParserError: $message';	
}

class PlusParser extends CombinatorialParser {
/* An attempt to optimize the + operator by having a dedicated parser for it. */
  CombinatorialParser p;
  
  PlusParser(this.p);

// parsing
   parseWithContext(ParserContext context) {
      int currentPos; 
      var nextResult;

      var results = new List();
      results.add (p.parseWithContext(context));
      while (true){
        currentPos = context.position;
        try {
           nextResult =  p.parseWithContext(context);
        } on ParserError catch (e) {
            context.position = currentPos;
            return results; 
          };
        results.add(nextResult);
    };
  }
}

class PredicateTokenParser extends CombinatorialParser {
/* Parses a single token matching a given
predicate. */

	   var predicate; 
	   var errMsg;

	PredicateTokenParser(this.predicate, this.errMsg);


// parsing
   parseWithContext(ParserContext context) {
    	var t = context.nextIfAbsent(() => throw new ParserError(errMsg, context.position-1));
    	if (!predicate(t))  throw new ParserError(errMsg, context.position-1);
     	return t;
   }
}


class SequentialParser extends CombinatorialParser {
/* A parser that activates a sequence of subparsers (P1, ... ,Pn).

One might think that it would be sufficient to define a class that
combined two parsers in sequence, corresponding to the <<
operator, just like AlternatingParser corresponds to the | operator.
However, grammar productions typically involve several elements, so
the typical sequencing operation is n-ary */

  List<CombinatorialParser> subparsers;
  SequentialParser(this.subparsers);

// combinators
  SequentialParser operator & (CombinatorialParser p) {
      List<CombinatorialParser> o = new List<CombinatorialParser>()..addAll(subparsers)..add(p); 
  		return new SequentialParser(o);
  }


// parsing 
   parseWithContext(ParserContext context) {
      return subparsers.map((p) =>  p.parseWithContext(context)).toList();
   }

// wrapping
   wrapper(blk) {
  
  /* untypesafe, but convenient. We can dynamically ensure
  that the arity of the incoming block matches that of this parser.
  Given that this routine is only called during parser construction,
  dynamic failure of the asserts is sufficient.

  We cannot ensure type correctness of the arguments to the block using
  this interface. One can use the more verbose followedBy: combinators
  if that is deemed essential.
  */
    //assert(blk.numArgs ==  subparsers.length);
    return wrap((rs) => Function.apply(blk, rs)); 
  }
}

class StarParser extends CombinatorialParser {
/* An attempt to optimize the * operator by having a dedicated parser for it. */

   CombinatorialParser p;

   StarParser(this.p);
// parsing
   parseWithContext(ParserContext context) {
      var currentPos;
      var nextResult;

      var results = new List();

      while (true) {
        currentPos = context.position;
        try {
          nextResult =  p.parseWithContext(context);
        } on ParserError catch (e) {
	           context.position = currentPos;
             return results;
          };
        results.add(nextResult);
      };
  }
}

class SymbolicTokenParser extends CombinatorialParser {
/* Parses a given symbol.  One could derive this as an alternation of character parsers, but the derivation is more verbose 
 * than defining it directly, and less efficient, so why bother? 
 * */
  String symbol;

  SymbolicTokenParser(this.symbol);

// parsing
  parseWithContext(ParserContext context) {
  	var errMsg =  '$symbol expected';
  	var pos = context.position;
  	for (var i = 0; i < symbol.length; i++) {
  	  var c = symbol[i];
  	  if (!(c == (context.nextIfAbsent(() => throw new ParserError(errMsg, pos)))))
        throw new ParserError(errMsg, pos);
  	}
 //   symbol.forEach((c){ if (!(c == (context.nextIfAbsent(() => throw new ParserError(errMsg, pos)))))
  //    throw new ParserError(errMsg, pos);}); //?
    return symbol;
  }
}

class Token {
/* Represents a token of input. Basically, it attaches a start position
to the token's value. 

It's not yet clear if we should bother adding token codes or values here. */

     var token;
     int start, end;

    bool operator == (other) {
    	 return other.runtimeType == this.runtimeType
		&& other.token == token
		&& other.start == start
		&& other.end ==  end;
    }

    int get concreteEnd => end;

    int get concreteStart => start;

    int get hashCode => token.hashCode;
    
    toString() => 'Token $token $start:$end';

   Token(this.token, this.start, this.end);

}

class TokenParser extends PredicateTokenParser {
/* A parser that accepts a single, specified token.
*/

   TokenParser(tokenToParse): super((t) => t == tokenToParse, '${tokenToParse} expected');
}

class TokenizingParser extends CombinatorialParser { 
  CombinatorialParser parser;

  TokenizingParser(this.parser);

// parsing
	parseWithContext(ParserContext context) {
	       var pos = context.position + 1;
	       var res =  parser.parseWithContext(context);
	       return new Token(res, pos, context.position);
  }
}

class WhitespaceParser extends CombinatorialParser {
/* A simple scanner to optimize the handling of whitespace. Should be equivalent to
 * 
 * aWhitespaceChar.plus
 * 
 * Eventually, the framework should optimize well enough that this will be unnecessary. 
 * */
  var comment;
// parsing
  parseWithContext(ParserContext context) {
    int pos = context.position;
	  comment =  new List();
	  for (String c = context.peekIfAbsent((){}); c == null ? false : c.codeUnitAt(0) <= 32; c = context.peekIfAbsent((){}))
	   {comment.add(context.next);};
	  if (comment.isEmpty) throw new ParserError('Whitespace expected', pos);
	  return comment;
  }
}

class WrappingParser extends CombinatorialParser {
/* Used to transform the output of another parser. A wrapping parser accepts exactly the same input as the wrapped
parser does, and performs the same error handling. The only difference is that it takes the output of the wrapped
parser and passes it on to a wrapper block which uses it to produce a new result, which is the output of the wrapping
parser. A typical use is to build nodes of an abstract syntax tree.

The output type of the wrapped parser, S, is also the input to the wrapper. The output type of the wrapper is the output of this 
(the wrapping) parser. */

     var parser;
     var wrapperBlock;

     WrappingParser(this.parser, this.wrapperBlock);

// parsing
   parseWithContext(ParserContext context) {
  	return wrapperBlock(parser.parseWithContext(context));
   }
}

class ForwardingWrappingParser extends WrappingParser {
/* When a ForwardingReferenceParser is wrapped using the wrapper() combinator, we don't know what the arity 
 * the wrapping function should have - it will depend on the arity of the parser we forward to.  
 * We cannot determine whether to use the implementation of wrapper() given in ordinary parsers, which forwards 
 * to the wrap() combinator (designed for functions with arity 1) or the implementation used in SequentialParsers, 
 * (designed for n-ary functions, where n is the length of the list of parsers the SequentialParser sequences). 
 * Instead, we must defer the decision on how to handle the situation until the parser tree is complete. 
 * This is accomplished by using this class as the result of the wrapper() combinator for ForwardReferenceParser.  
 * 
 * Instances of this class determine how to act when asked to parse. At that time, the parse tree must be complete, 
 * and they can ask the ultimate parser for a wrapping parser that is suitable configured, and forward requests 
 * to it. */
	var wrappingParser;

  ForwardingWrappingParser(p, b): super(p, b); // would a named constructor like ForwardingWrappingParser.wrap() be nicer here?

   parseWithContext(ParserContext context) {
   	  return trueWrappingParser.parseWithContext(context);
   }

   WrappingParser get trueWrappingParser {
    	if (wrappingParser == null)
    		wrappingParser = parser.ultimateParser.wrapper(wrapperBlock);
    	return wrappingParser;
    }
}

class ForwardReferenceParser extends CombinatorialParser {
      var forwardee; 
      var bindingRoutine; 

   bind(CombinatorialParser p) {
   	if (p is CombinatorialParser) forwardee = p; /* as a precaution, only bind if p is a parser */
  }

    CombinatorialParser get parserToForwardTo {
    	if (forwardee == null) bindingRoutine();
    	return forwardee;
    }

    CombinatorialParser get ultimateParser => parserToForwardTo.ultimateParser;

    CombinatorialParser wrapper(blk) {
    /* see comments in ForwardingWrappingParser */
      return new ForwardingWrappingParser(this, blk);
    }

// combinators
   CombinatorialParser operator << (CombinatorialParser p) {
   		       return forwardee == null ? super << p : forwardee  << p;
   }

   CombinatorialParser get opt {
       return (forwardee == null) ? super.opt : forwardee.opt;
   }

   CombinatorialParser operator |(p) {
   		       return (forwardee == null) ? super | p : forwardee | p;
   }

// parsing
   parseWithContext(ParserContext context) {
   	  return parserToForwardTo.parseWithContext(context);
   }
}

abstract class ExecutableGrammar extends CombinatorialParser {
/* This class is intended to implement Parser
Combinators. A ExecutableGrammar[T]
returns a value of type T after successful
parsing.

The class is abstract. It does not implement
 the parsing routine parseWithContext().

Concrete subclasses should implement specific grammars.

Parsing is initiated by calling parse(). This routine takes a String as input.
If parsing fails, it is the caller's responsibility to set the input stream back to its original position
(Q: is this a good idea?).
If an error occurs, the error block passed in is called. */

	Map forwardReferenceTable = new Map();
	InstanceMirror selfMirror;
  
  ExecutableGrammar() {
    // Broken - now that asynchrony is enforced, one cannot rely on the mirror API to get things done
    // at any specific time. Fixing this would change client grammars and make their API async as well!
  		      selfMirror = reflect(this); 
  		      setupForwardReferences;
  		      bindForwardReferences;
  }

// forward references
   get bindForwardReferences {
    forwardReferenceTable.values.forEach((v){v.bindingRoutine = () => finalBindForwardReferences;});
   }

   
   
   Future<CombinatorialParser> getRealField(String k){
     /* A gross hack to work around the deficiencies in Dart's mirror lib.
      * If the current class has a getter k but not a field
      * then assume that its getter k overrides the production k stored
      * in a field in the superclass with a wrapping parser on the field contents.
      * */
     return selfMirror.getField(k).then(
         (value) {
           var p = value.reflectee;
           if (selfMirror.type.getters.containsKey(k) && !selfMirror.type.variables.containsKey(k))
           return p.parser;
           else return p;
         });
   }
   
   
   get finalBindForwardReferences { // we could do this more eagerly after grammar was constructed, chaining it to
     // gramamr construction. The result would be a future that one could chain to in order to parse
     // and the results of parsing would also be futures. Call back hell.
        forwardReferenceTable.forEach((k, v){
//			      var p = selfMirror.getField(k).value.reflectee;
            getRealField(k).then(
                (p) { 
                  
			            if (p is CombinatorialParser) {
			      	      v.bind(p);
				            p.name = k; /* a good place to name the productions */
			            }}
			      );
        });
   }
   
   // helper methods because Dart mirrors do not climb the class hierarchy; unused
   
   List<ClassMirror> get _allSuperClasses {
     List<ClassMirror> allSuperclasses = new List();
     ClassMirror sc = selfMirror.type;
     while (sc.simpleName != 'Object') {
       allSuperclasses.add(sc);
       sc = sc.superclass;
     }
     return allSuperclasses;
   }
   
   List<VariableMirror> get _allSlotMirrors {
     List<VariableMirror> allSlots = new List();
     _allSuperClasses.forEach((sc) => allSlots.addAll(sc.variables.values));
     return allSlots;
   }
   
   List<VariableMirror> get _allProductions {
     List<VariableMirror> allProductions = new List();
     ClassMirror gc = selfMirror.type;
     while (gc.simpleName != 'ExecutableGrammar') {
       allProductions.addAll(gc.variables.values);
       gc = gc.superclass;
     }
     return allProductions;      
   }

   get setupForwardReferences { 
     /* go thru all non-nil instance variables and set them to a fresh forward reference */
     /* If these do not correspond to productions, they will be overridden by the subclass */
     _allProductions.forEach((VariableMirror slot){ 
	       String iv = slot.simpleName;
	       var fref =  new ForwardReferenceParser();
	       /* Change due to deficiency in Dart mirror lib. Since getField invokes the getter rather than
	        * actually accessing the field, once we override a production, the original code no longer works.  
	        * Instead, assume that all slots that are defined in subclasses of ExecutableGrammar are productions.
	        * They will get overridden if they have another use. Only if a field with the same name is defined by a
	        * subclass will this fail.
	        *  */
//	       if ((selfMirror.getField(iv)).value.reflectee == null) {
		      forwardReferenceTable[iv] = fref;
		      /* set iv to fref */
		      selfMirror.setField(iv, reflect(fref)); 
		      // would need to chain all these, and chain grammar construction to the result. This means grammar would have to be
		      // in a known abstract method called by the constructor of this class.
//		    };
	   });	 
   }
}
   

abstract class RunnableGrammar extends CombinatorialParser {
/* This class is intended to implement Parser
Combinators. A RunnableGrammar[T]
returns a value of type T after successful
parsing.

The class is abstract. It does not implement
 the parsing routine parseWithContext().

Concrete subclasses should implement specific grammars.

Parsing is initiated by calling parse(). This routine takes a String as input.
If parsing fails, it is the caller's responsibility to set the input stream back to its original position
(Q: is this a good idea?).

This class differs from ExecutableGrammar in that it does not rely on reflection for its implementation. 
Instead, it makes heavy use of noSuchMethod(). The advantage is that users do not have to declare all productions 
as fields before actually setting the production value in the constructor.
There is also a temporary advantage that this version can be used with dart2js right now, before mirrors are
implemented; it may be that even when mirrors are supported by dart2js, the space advantages may be significant.

The disadvantages are that parsing is slower, because productions are looked up via noSuchMethod() instead
of as fields, and the absence of type information.
 */

  Map<String, ForwardReferenceParser> forwardReferenceTable = new Map<String, ForwardReferenceParser>();
  Map<String, dynamic> productions = {}; // non-parsers might be inserted by noSuchMethod, so values have type dynamic
  
  RunnableGrammar();
  
  noSuchMethod(Invocation im){
    // if its a get, look it up in the productions map; if its missing, install a forwarder and return that
    if (im.isGetter) {
      var result;
      return (result = productions[im.memberName]) == null ? setupForwardReference(im.memberName): result;
    };
    // if it is a set, install in the productions map
    if (im.isSetter){
      // Must be careful, since setter name and getter name differ by trailing '='!
      String setterName = MirrorSystem.getName(im.memberName);
      Symbol fieldName = new Symbol(setterName.substring(0, setterName.length - 1));
      return productions[fieldName] = im.positionalArguments[0];      
    };
    // otherwise forward to super method
    return super.noSuchMethod(im);
  }

  
  ForwardReferenceParser setupForwardReference(String productionName){
    ForwardReferenceParser fref =  new ForwardReferenceParser();
    fref.bindingRoutine = () => finalBindForwardReferences;
    return productions[productionName] = forwardReferenceTable[productionName] = fref;
  }
  
  
// forward references
   
   get finalBindForwardReferences {
        forwardReferenceTable.forEach((k, v){
            var p = productions[k];
            if (p is CombinatorialParser) {
               v.bind(p);
               p.name = k; /* a good place to name the productions */
            }});
   }
}
