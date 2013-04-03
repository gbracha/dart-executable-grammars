/* 
   Copyright 2008 Cadence Design Systems, Inc.
   Copyright 2013 Google Inc.
   
   Licensed under the Apache License, Version 2.0 (the ''License''); 
   you may not use this file except in compliance with the License.  
   You may obtain a copy of the License at  http://www.apache.org/licenses/LICENSE-2.0
*/


library smalltalk_revised_parser;
import 'CombinatorialParsing.dart';
import 'smalltalkGrammarRevised.dart';


class StringAST extends AST {
  String val;
  StringAST(this.val);
}


class VarDeclAST extends AST {
  var name, type;
  VarDeclAST(this.name, this.type);
}



class PackageAST extends AST {
  var name, classes, extendedClasses;
  PackageAST(this.name, this.classes, this.extendedClasses);
}

class MessageAST extends AST {
  var sel, args;
  MessageAST(this.sel, this.args) {
    assert(sel is String);
    assert(args is Collection);
  }
}


class MethodAST extends AST {
  var pattern, body, visibility;
  MethodAST(this.pattern, this.body, this.visibility);
}

class AST {
  var start, end, cStart, cEnd;

  get concreteEnd => cEnd == null ? end : cEnd;
  set concreteEnd(p) => cEnd = p;
  get concreteStart => cStart == null ? start : cStart;
  set concreteStart(p) => cStart = p;
}

class SideAST extends AST {
  var vars, categories, classComment, classPoolVars, sharedPoolVars;
}

class ClassAST extends AST {
  var format, name, superclassName, instanceSide, classSide, category;
}

class MessagePatternAST extends AST {
  var selector, parameters, returnType, typePattern;
  MessagePatternAST(this.selector, this.parameters);
}

class CategoryAST extends AST {
  var name, methods;
  CategoryAST(this.name, this.methods);
}




class SmalltalkParser extends SmalltalkGrammarRevised {
  get binaryMsgPattern {
   return super.binaryMsgPattern.wrapper((Token sel, p) {
                var params = new List();
                params.add(p);
                return new MessagePatternAST(sel.token, params)
                    ..start = sel.start 
                    ..end = p.end;            
               });
  }

  get category {
    return super.category.wrapper((StringAST cn, List<AST> ms) {
                      var end = ms.isEmpty ? cn.end : ms.last.end;
                      return new CategoryAST(cn.val, ms)
                       ..start = cn.start 
                       ..end = end;
                      });
  }

String charsToString(List<String> cs) => cs.reduce('', (s, c) => '$s $c');


  get classBody {
    return super.classBody.wrapper((Token lp, cmnt, instVars, classPoolVars, sharedPoolVars, List categories, Token rp) {
                  var vs = instVars == null? new List() : instVars;
                  return new SideAST()
                    ..vars = vs 
                    ..categories = categories 
                    ..classComment = cmnt 
                    ..start = lp.start 
                    ..end = rp.end 
                    ..classPoolVars = classPoolVars
                    ..sharedPoolVars = sharedPoolVars;                  
                  });
  }

  get classComment {
    return super.classComment.wrapper((ws, c) => (c[2]) .reduce('', (s, ch) => '$s$ch'));
  }

  get classDefinition {
    return super.classDefinition.wrapper((cat, format, className, eq, superClassName, instSide, classSide) {
                 var klassSide = classSide == null ? (new SideAST()..vars = new List() ..categories = new List())
                                                   : classSide;
                 var kat = cat == null ? 'Newsqueak-tests' : cat.val;
                 return new ClassAST()
                    ..name = className.snd 
                    ..superclassName = superClassName.snd 
                    ..instanceSide = instSide
                    ..classSide = klassSide
                    ..start = className.start
                    ..end = (classSide == null ? instSide.end :classSide.end)
                    ..category = kat 
                    ..format = format.token;           
                });
  }

  get classSideDecl => super.classSideDecl.wrapper((cn, side) => side);

  get extendedClass {
   return super.extendedClass.wrapper((extensions, name, eq, lp, instCategories, rp, colon, lp2, classCategories, rp2) =>
    new ClassAST()
      ..name = name.snd
      ..instanceSide = (new SideAST()..categories = instCategories)
      ..classSide = (new SideAST()..categories = classCategories)
     );
  }

  get keywordMsgPattern {
    return super.keywordMsgPattern.wrap((List kws) {
             var params =  new List();
             assert(!kws.isEmpty);
             var sel =  kws.reduce('', (s , kwp) { 
                                           params.add(kwp.last);
                                           '$s ${kwp.first.token}';
                                          });
              return new MessagePatternAST(sel, params) 
                                          ..start = kws.first.first.start
                                          ..end = params.last.end;  
            });
  }

  get method {
    return super.method.wrapper((msg, cb, ei) =>
                   new MethodAST(msg, cb,'public')
                        ..start = msg.start
 //                       ..end = cb.end
 //                       ..concreteEnd = cb.concreteEnd                         
                    );
  }

  get methodDecl {
    return super.methodDecl.wrapper((msg, eq, lp, cb, rp) =>                                                       
                   new MethodAST(msg, cb,'public') 
                    ..start = msg.start
                    ..end = rp.end                                                                                     
                );
  }

  get package {
   return super.package.wrapper((langId, pkg, name, eq, lp, classes, extendedClasses, rp, end) =>
    new PackageAST(name.val,
      classes == null ? new List(): classes,
      extendedClasses == null ? new List(): extendedClasses
      )
     );
  }

  get string {
    return super.string.wrap((t) => new StringAST(t.token)
          ..start = t.start
          ..end = t.end);
  }

  get unaryMsgPattern {
    return super.unaryMsgPattern.wrap((sel)  =>
                    new MessagePatternAST(sel.sel, new List())
                                               ..start = sel.start.. end = sel.end   
                    );
  }

  get unarySelector {
    return super.unarySelector.wrap((u) => 
              new MessageAST(u.token, new List())
              ..start = u.start
              ..end = u.end 
            );
  }

  get varDecls {
    return super.varDecls.wrapper((vb1, vds, vb2) { 
                  if (!vds.isEmpty) {
                    vds.first.concreteStart = vb1.start;
                    vds.last.concreteEnd = vb2.end;
                  };
                  return vds;
                });
  }

  get variableDecl {
    return super.variableDecl.wrapper((Token n) =>
                 new VarDeclAST(n.token, null)
                  ..start = n.start
                  ..end = n.end
        );
  }

}