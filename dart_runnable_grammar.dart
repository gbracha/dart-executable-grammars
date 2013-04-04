/* 
Copyright 2013 Google Inc.

Licensed under the Apache License, Version 2.0 (the ''License''); 
you may not use this file except in compliance with the License.  
You may obtain a copy of the License at  
http://www.apache.org/licenses/LICENSE-2.0 
*/

library dart_grammar;
import 'CombinatorialParsing.dart';


class DartGrammar extends RunnableGrammar {
  
  DartGrammar() {
    //lexemes

    backSlash = token('\\');
    colon = token(':');
    comma = token(',');
    dollar = token('\$');  
    dot = token('.');
    doubleQuote = token('"');
    equalSign = token('=');
    lparen = token('(');
    minus = token('-');
    plus = token('+');  
    rparen = token(')');
    semicolon = token(';');
    singleQuote = token("'");
    tripleDoubleQuote = token('"""');
    tripleSingleQuote = token("'''");
    
    DIGIT = charBetween('0', '9');
    LETTER = new PredicateTokenParser((c) => isLetter(c), 'letter expected');
    HEX_DIGIT = 
        charBetween('a','f')
        | charBetween('A','F')
        | DIGIT
        ;
    
    HEX_NUMBER =
        token('0x') & HEX_DIGIT.plus
    | token('0X') & HEX_DIGIT.plus
    ;
    
    EXPONENT =
      (token('e') | token('E')) & (plus | minus).opt & DIGIT.plus
    ; 
    
    NUMBER =
      DIGIT.plus & (dot & DIGIT.plus).opt & EXPONENT.opt
    |  dot & DIGIT.plus & EXPONENT.opt
    ;

    HEX_DIGIT_SEQUENCE =
      HEX_DIGIT & HEX_DIGIT.opt & HEX_DIGIT.opt & HEX_DIGIT.opt & HEX_DIGIT.opt & HEX_DIGIT.opt; 

    ESCAPE_SEQUENCE =
        token('\n')
      | token('\r')
      | token('\f')
      | token('\b')
      | token('\t')
      | token('\v')
      | token('\\x') & HEX_DIGIT & HEX_DIGIT
      | token('\\u') & HEX_DIGIT & HEX_DIGIT & HEX_DIGIT & HEX_DIGIT
      | token('\\u{') & HEX_DIGIT_SEQUENCE & token('}')
      ;





    
    //keywords
    ASSERT = token('assert');
    BREAK = token('break'); 
    CASE = token('case');
    CATCH = token('catch'); 
    CLASS = token('class'); 
    CONST = token('const');
    CONTINUE = token('continue');
    DEFAULT = token('default');
    DO = token('do');
    ELSE = token('else');
    EXTENDS = token('extends');
    FALSE = token('false');
    FINAL = token('final');
    FINALLY = token('finally');
    FOR = token('for');
    IF = token('if');
    IN = token('in');
    IS = token('is');
    NEW = token('new');
    NULL = token('null');
    RETHROW = token('rethrow');
    RETURN = token('return');
    SUPER = token('super');
    SWITCH = token('switch');
    THIS = token('this');
    THROW = token('throw');
    TRUE = token('true');
    TRY = token('try');
    VAR = token('var');
    VOID = token('void');
    WHILE = token('while');
    WITH = token('with');
    
    // built-in identifiers
    
    ABSTRACT = token('abstract');
    AS = token('as');
    DYNAMIC = token('dynamic');
    EXPORT = token('export');
    EXTERNAL = token('external');
    FACTORY = token('factory');
    GET = token('get');
    IMPLEMENTS = token('implements');
    IMPORT = token('import');
    LIBRARY = token('library');
    OPERATOR = token('operator');
    PART = token('part');
    SET = token('set');
    STATIC = token('static');
    TYPEDEF = token('typedef');
    
    // special cases
    HIDE = token('hide');    
    SHOW = token('show');
    OF = token('of');
    ON = token('on'); 
    //syntax
    
    // types
    
    typeArguments = token('<') & typeList & token('>');
    typeName = qualified;
    type = typeName & typeArguments.opt;
    typeList = type.starSeparatedBy(comma);
    functionPrefix = returnType.opt & identifier;
    functionTypeAlias = functionPrefix & typeParameters.opt & formalParameterList & semicolon;
    typeAliasBody = identifier & typeParameters.opt & equalSign & ABSTRACT.opt & mixinApplication | functionTypeAlias;
    typeAlias = metadata & TYPEDEF & typeAliasBody;
    
    // declarations
    
    metadata = (token('@') & qualified & (dot & identifier).opt & arguments.opt).star;
    
    typeParameter = metadata & identifier & (EXTENDS & type).opt;
    typeParameters = token('<') & typeParameter.plusSeparatedBy(comma) & token('>');
 
    returnType:
      VOID
      | type
    ; 
 
    varOrType =
        VAR
        | type
        ;

    finalConstVarOrType =
      FINAL & type.opt
      | CONST & type.opt
      | varOrType
    ;

    declaredIdentifier = metadata & finalConstVarOrType & identifier;
    variableDeclaration = declaredIdentifier.plusSeparatedBy(comma);
    initializedIdentifier = identifier & (equalSign & expression).opt;
    initializedVariableDeclaration = declaredIdentifier & (equalSign & expression).opt & (comma & initializedIdentifier).star;
    initializedIdentifierList = initializedIdentifier.plusSeparatedBy(comma);

    
    fieldFormalParameter = metadata & finalConstVarOrType.opt & THIS & dot & identifier; 
    
    simpleFormalParameter =
      declaredIdentifier
    | metadata & identifier
    ;
    
    normalFormalParameter =
      functionSignature
    | fieldFormalParameter
    | simpleFormalParameter
    ;

    normalFormalParameters = normalFormalParameter.plusSeparatedBy(comma);
    defaultFormalParameter = normalFormalParameter & (equalSign & expression).opt;
    defaultNamedParameter = normalFormalParameter & (colon & expression).opt;
    optionalPositionalFormalParameters = token('[') & defaultFormalParameter.plusSeparatedBy(comma) & token(']');
    
    optionalFormalParameters =
      optionalPositionalFormalParameters |
      namedFormalParameters
    ;

    formalParameterList =     
      lparen & rparen 
      | lparen & normalFormalParameters & (comma & optionalFormalParameters).opt & rparen
      | lparen & optionalFormalParameters & rparen
    ; 

    namedFormalParameters = token('{') & defaultNamedParameter.plusSeparatedBy(comma) & token('}');
    
    functionSignature = metadata & returnType.opt & identifier & formalParameterList;
    block = token('{') & statements & token('}');

    functionBody =
      token('=>') & expression & semicolon
      | block
    ;


    interfaces = IMPLEMENTS & typeList;
    superclass = EXTENDS & type;
    ;
    
    constantConstructorSignature = CONST & qualified & formalParameterList;
    redirectingFactoryConstructorSignature =
      CONST.opt & FACTORY & identifier & (dot & identifier).opt &  formalParameterList & equalSign & type & (dot & identifier).opt
    ;
    factoryConstructorSignature =
        FACTORY & identifier & (dot & identifier).opt & formalParameterList
    ;
    

    fieldInitializer = (THIS & dot).opt & identifier & equalSign & conditionalExpression & cascadeSection.star;
    superCallOrFieldInitializer =
      SUPER & arguments
      | SUPER & dot & identifier & arguments
      | fieldInitializer
    ;
    
    initializers = colon & superCallOrFieldInitializer.plusSeparatedBy(comma); 
    redirection = colon & THIS & (dot & identifier).opt & arguments;
    constructorSignature = identifier & (dot & identifier).opt & formalParameterList;
    setterSignature = returnType.opt & SET & identifier & formalParameterList;
    getterSignature = type.opt & GET & identifier;
    
    binaryOperator =
      multiplicativeOperator
    | additiveOperator
    | shiftOperator
    | relationalOperator
    | token('==')
    | bitwiseOperator
    ;

    operator =
      token('~')
    | binaryOperator
    | token('[') & token(']')
    | token('[') & token(']') & equalSign
    ;
    
    operatorSignature = returnType.opt & OPERATOR & operator & formalParameterList;
    
    mixins = WITH & typeList;
    
    methodSignature =
      constructorSignature & initializers.opt
    | factoryConstructorSignature
    | STATIC.opt & functionSignature
    | STATIC.opt & getterSignature
    | STATIC.opt & setterSignature
    | operatorSignature
    ;
    
    staticFinalDeclaration = identifier & equalSign & expression;
    staticFinalDeclarationList = staticFinalDeclaration.plusSeparatedBy(comma);

    declaration =
      constantConstructorSignature & (redirection | initializers).opt
    | constructorSignature & (redirection | initializers).opt
    | EXTERNAL & constantConstructorSignature
    | EXTERNAL & constructorSignature
    | EXTERNAL & factoryConstructorSignature
    | (EXTERNAL & STATIC.opt).opt & getterSignature
    | (EXTERNAL & STATIC.opt).opt & setterSignature
    | EXTERNAL.opt & operatorSignature
    | (EXTERNAL & STATIC.opt).opt & functionSignature
    | getterSignature
    | setterSignature
    | operatorSignature
    | functionSignature
    | STATIC & (FINAL | CONST) & type.opt & staticFinalDeclarationList
    | CONST & type.opt & staticFinalDeclarationList  
    | FINAL & type.opt & initializedIdentifierList
    | STATIC.opt & (VAR | type) & initializedIdentifierList
    ;



    classMemberDefinition =
      declaration & semicolon
      | methodSignature & functionBody
    ;
    
    classDefinition =
        metadata & ABSTRACT.opt & CLASS & identifier & typeParameters.opt & (superclass & mixins.opt).opt & interfaces.opt
        & token('{') & (metadata & classMemberDefinition).star & token('}')
        ;
    
    mixinApplication = type & mixins & interfaces.opt;




    namedArgument = label & expression;
    argumentList =
        namedArgument.plusSeparatedBy(comma)
        | expressionList.plusSeparatedBy(comma)
    ;
   
    arguments = lparen & argumentList.opt & rparen;





    
    // expressions
    
    IDENTIFIER_START_NO_DOLLAR =
      LETTER
    | token('_')
    ;

    IDENTIFIER_START =
      IDENTIFIER_START_NO_DOLLAR
    | dollar
    ;

    IDENTIFIER_PART_NO_DOLLAR =
      IDENTIFIER_START_NO_DOLLAR
    | DIGIT
    ;


    IDENTIFIER_PART =
      IDENTIFIER_START
    | DIGIT
    ;

    IDENTIFIER_NO_DOLLAR = IDENTIFIER_START_NO_DOLLAR & IDENTIFIER_PART_NO_DOLLAR.star;

    IDENTIFIER = IDENTIFIER_START & IDENTIFIER_PART.star;
    
    identifier = IDENTIFIER;
    qualified = identifier.plusSeparatedBy(dot);
    
    assignableSelector =
      token('[') & expression & token(']')
    | dot & identifier
    ;
    
    assignableExpression =
      primary & (arguments.star & assignableSelector).plus
    | SUPER & assignableSelector
    | identifier
    ;

    incrementOperator = token('++') | token('--');
    selector = assignableSelector | arguments;
    postfixOperator = incrementOperator;
    
    postfixExpression =
      assignableExpression & postfixOperator
    | primary & selector.star
    ;
    
    unaryOperator = token('!') | token('~');
    prefixOperator = minus | unaryOperator;
    unaryExpression =
      prefixOperator & unaryExpression
    | postfixExpression
    | prefixOperator & SUPER
    | incrementOperator & assignableExpression
    ;
    
    multiplicativeOperator = token('*') | token('/') | token('%') | token('~/');
    multiplicativeExpression =
      unaryExpression & (multiplicativeOperator & unaryExpression).star
    | SUPER & (multiplicativeOperator & unaryExpression).plus
    ;

    additiveOperator = plus | minus;
    
    additiveExpression =
      multiplicativeExpression & (additiveOperator & multiplicativeExpression).star
    | SUPER & (additiveOperator & multiplicativeExpression).plus
    ;
    
    shiftOperator = token('<<') | token('>>');
    shiftExpression =
      additiveExpression & (shiftOperator & additiveExpression).star
    | SUPER & (shiftOperator & additiveExpression).plus
    ;
    
    relationalOperator = token('<') | token('>') | token('<=') | token('>=');
    relationalExpression =
      shiftExpression & (typeTest | typeCast | relationalOperator & shiftExpression).opt
    | SUPER & relationalOperator & shiftExpression
    ;
    
    equalityOperator = token('==') | token('!=');
    equalityExpression =
      relationalExpression & (equalityOperator & relationalExpression).opt
    | SUPER & equalityOperator & relationalExpression
    ;

    bitwiseOperator = token('|') | token('&') | token('^');
    bitwiseAndExpression =
      equalityExpression & (token('&') & equalityExpression).star
    | SUPER & (token('&') & equalityExpression).plus
    ;
    bitwiseXorExpression =
      bitwiseAndExpression & (token('^') & bitwiseAndExpression).star
    | SUPER & (token('^') & bitwiseAndExpression).plus
    ;
    bitwiseOrExpression =
        bitwiseXorExpression & (token('|') & bitwiseXorExpression).star
        | SUPER & (token('|') & bitwiseXorExpression).plus
        ;
    
    logicalAndExpression = bitwiseOrExpression & (token('&&') & bitwiseOrExpression).star;
    logicalOrExpression = logicalAndExpression & (token('||') & logicalAndExpression).star;
    
    conditionalExpression = logicalOrExpression & (token('?') & expressionWithoutCascade & colon & expressionWithoutCascade).opt;
    
    compoundAssignmentOperator =
      token('*=')
    | token('/=')
    | token('~/=')
    | token('%=')
    | token('+=')
    | token('-=')
    | token('<<=')
    | token('>>=')
    | token('&=')
    | token('^=')
    | token('|=')
    ;
    assignmentOperator = equalSign | compoundAssignmentOperator;

    cascadeSelector =
      token('[') & expression & token(']')
      | identifier
      ;
    cascadeSection =
      token('..')  &
      (cascadeSelector & arguments.star) & 
      (assignableSelector & arguments.star).star & 
      (assignmentOperator & expressionWithoutCascade).opt
    ;
    
    namedArgument = label & expression;
    argumentList =
      namedArgument.plusSeparatedBy(comma)
    | expressionList & (comma & namedArgument).star
    ;
    arguments = lparen & argumentList.opt & rparen;
    
    isOperator = IS & token('!').opt;
    typeTest = isOperator & type;
    typeCast = AS & type;
    argumentDefinitionTest = token('?') & identifier;
    
    constObjectExpression = CONST & type & (dot & identifier).opt & arguments;
    newExpression = NEW & type & (dot & identifier).opt & arguments;
    
    thisExpression = THIS;
      
    functionExpressionBody =
      token('=>') & expression
    | block
    ;
    functionExpression = formalParameterList & functionExpressionBody;
      
    rethrowExpression = RETHROW;
    throwExpression = THROW & expression;
    throwExpressionWithoutCascade = THROW & expressionWithoutCascade;
    
    mapLiteralEntry = stringLiteral & colon & expression;
    mapLiteral =
      CONST.opt & 
      typeArguments.opt & 
      token('{') & 
      (mapLiteralEntry & (dot & mapLiteralEntry).star & comma.opt).opt & 
      token('}');
      
    listLiteral =
        CONST.opt & typeArguments.opt & token('[') & (expressionList & comma.opt).opt & token(']');
    
    stringInterpolation = dollar & IDENTIFIER_NO_DOLLAR |
        dollar & token('{') & expression & token('}');
    NEWLINE = token('\\n') | token('\r');
    stringContentDQ = (backSlash | doubleQuote | dollar | NEWLINE).not |
        backSlash & NEWLINE.not |
        stringInterpolation;
    stringContentSQ = (backSlash | singleQuote | dollar | NEWLINE).not |
        backSlash & NEWLINE.not |
        stringInterpolation;
    stringContentTDQ = (backSlash | tripleDoubleQuote | dollar | NEWLINE).not |
        backSlash & NEWLINE.not |
        stringInterpolation;
    stringContentTSQ = (backSlash | tripleSingleQuote | dollar | NEWLINE).not |
        backSlash & NEWLINE.not |
        stringInterpolation;

    multilineString =
      tripleDoubleQuote & stringContentTDQ.star & tripleDoubleQuote
      | tripleSingleQuote & stringContentTSQ.star & tripleSingleQuote
      | token('r') & tripleDoubleQuote & doubleQuote.not.star & tripleDoubleQuote
      | token('r') & tripleSingleQuote & singleQuote.not.star & tripleSingleQuote
    ;
    
    singleLineString =
        doubleQuote & stringContentDQ.star & doubleQuote
        | singleQuote & stringContentSQ.star & singleQuote
        | token('r') & doubleQuote & ( doubleQuote | NEWLINE ).not.star & doubleQuote
        | token('r') & singleQuote & ( singleQuote | NEWLINE ).not.star & singleQuote
        ;

    
    stringLiteral =
      multilineString.plus
    | singleLineString.plus
    ;
    
    numericLiteral =
      NUMBER
      | HEX_NUMBER
    ;
    
    booleanLiteral =
      TRUE
    | FALSE
    ;

    nullLiteral = NULL;

    literal =
      nullLiteral
    | booleanLiteral
    | numericLiteral
    | stringLiteral
    | mapLiteral
    | listLiteral
    ;

    expression =
      assignableExpression & assignmentOperator & expression
      | conditionalExpression & cascadeSection.star
      | throwExpression
      | rethrowExpression
    ;



    expressionWithoutCascade =
      assignableExpression & assignmentOperator & expressionWithoutCascade
      | conditionalExpression
      | throwExpressionWithoutCascade
      | rethrowExpression
    ;

    expressionList =  expression.plusSeparatedBy(comma);


    primary =
      thisExpression
      | SUPER & assignableSelector
      | functionExpression
      | literal
      | identifier
      | newExpression
      | constObjectExpression
      | lparen & expression & rparen
      | argumentDefinitionTest
    ;
    // statements
    
    assertStatement = ASSERT & lparen & conditionalExpression & rparen & semicolon;
    continueStatement = CONTINUE & identifier.opt & semicolon;    
    breakStatement = BREAK & identifier.opt & semicolon;
    label = identifier & colon;
    returnStatement = RETURN & expression.opt & semicolon;
    
    finallyPart = FINALLY & block;
    catchPart = CATCH & lparen & identifier & (comma & identifier).opt & rparen;
    onPart = 
        catchPart &  block
        | ON & type & catchPart.opt & block
    ;

    tryStatement = TRY & block & (onPart.plus & finallyPart.opt | finallyPart);
    
    defaultCase = label.star & DEFAULT & colon & statements;
    switchCase = label.star & (CASE & expression & colon) & statements;
    switchStatement = SWITCH & lparen & expression & rparen & token('{') & switchCase.star & defaultCase.opt & token('}'); 
    doStatement = DO & statement & WHILE & lparen & expression & rparen & semicolon;
    whileStatement = WHILE & lparen & expression & rparen & statement;
    
    forInitializerStatement =
      localVariableDeclaration & semicolon
    | expression.opt & semicolon
    ;
    
    forLoopParts =
      forInitializerStatement & expression.opt & semicolon & expressionList.opt
    | declaredIdentifier & IN & expression
    | identifier & IN & expression
    ;
    
    forStatement = FOR & lparen & forLoopParts & rparen & statement;
    ifStatement = IF & lparen & expression & rparen & statement & (ELSE & statement).opt;
    localFunctionDeclaration = functionSignature & functionBody;
    localVariableDeclaration = initializedVariableDeclaration & semicolon;
    expressionStatement = expression.opt & semicolon;
    
    nonLabelledStatement = 
          block
          | localVariableDeclaration & semicolon
          | forStatement
          | whileStatement
          | doStatement
          | switchStatement
          | ifStatement
          | tryStatement
          | breakStatement
          | continueStatement
          | returnStatement
          | expressionStatement
          | assertStatement
          | localFunctionDeclaration;

          
    statement = label.star & nonLabelledStatement;
    statements = statement.star;
    
    // libraries and scripts
    
    uri = stringLiteral;
    getOrSet = GET | SET;
    
    topLevelDefinition =
        classDefinition
        | mixinApplication
        | typeAlias
        | EXTERNAL & functionSignature
        | EXTERNAL & getterSignature
        | EXTERNAL & setterSignature
        | functionSignature & functionBody
        | returnType.opt & getOrSet & identifier & formalParameterList & functionBody
        | (FINAL | CONST) & type.opt & staticFinalDeclarationList & semicolon
        | variableDeclaration & semicolon
        ;

    identifierList = identifier.starSeparatedBy(comma);
    combinator =  
        SHOW & identifierList
        | HIDE & identifierList;
    
    
    libraryImport = metadata & IMPORT & (AS & identifier).opt & combinator.star & semicolon;
    libraryExport = metadata & EXPORT & uri & combinator.star & semicolon;
    importOrExport = libraryImport | libraryExport;

    libraryName = metadata & LIBRARY & identifier.plusSeparatedBy(dot) & semicolon;
    
    partDirective = metadata & PART & stringLiteral & semicolon;
    partHeader = metadata & PART & OF & identifier.plusSeparatedBy(dot) & semicolon;    
    partDeclaration = partHeader & topLevelDefinition.star & eoi;
    
    libraryDefinition = libraryName.opt & importOrExport.star & partDirective.star & topLevelDefinition.star;
    
    scriptTag = token('#!') & NEWLINE.not.star & NEWLINE;
    scriptDefinition = scriptTag.opt & libraryDefinition;
  }
  
 bool isLetter(c) {
    int
    code = c.charCodeAt(0);
    return (code >= 97 && code <= 122) || (code >= 65 && code <= 90);
  }  
}


