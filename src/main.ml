(**
 * "Core Java" Type Checker.
 * @author Antoniu Miclaus
 * @author Serban Petrescu
 * @license MIT
 *)

open List;;

(* AST definition *)

type primitiveType = IntegerType 
								 	 | FloatType 
									 | BooleanType 
									 | VoidType

type valueType = IntegerType 
						 	 | FloatType 
							 | BooleanType 
							 | VoidType
						   | ClassName of string 
							 | BottomType

type value = NullValue 
					 | IntegerValue of int 
					 | FloatValue of float 
					 | BooleanValue of bool
					 | VoidValue

type integerOperator = IntegerAddition 
									   | IntegerSubtraction 
										 | IntegerMultiplication 
										 | IntegerDivision

type floatOperator = FloatAddition 
								   | FloatSubtraction 
									 | FloatMultiplication 
									 | FloatDivision

type comparisonOperator = LowerThan
												| LowerOrEqualTo
												| EqualTo
												| NotEqualTo
												| GreaterThan
												| GreaterOrEqualTo

type binaryLogicalOperator = LogicalAnd 
													 | LogicalOr
													
type unaryLogicalOperator = LogicalNot

type expressionBlock = LocalVariableBlock of valueType * string * expression 
										 | ImmediateBlock of expression
										
and expression = Value of value
							 | Variable of string
							 | ObjectField of (string * string)
							 | ExpressionBlock of expressionBlock
							 | VariableAssignment of (string * expression)
							 | ObjectFieldAssignment of (string * string * expression)
							 | Sequence of expression list
							 | IfThenElse of (string * expressionBlock * expressionBlock)
							 | IntegerOperation of (expression * integerOperator * expression)
							 | FloatOperation of (expression * floatOperator * expression)
							 | LogicalBinaryOperation of (expression * binaryLogicalOperator * expression)
							 | LogicalUnaryOperation of (unaryLogicalOperator * expression)
							 | ObjectInstantiation of (string * (expression list))
							 | MethodCall of (string * string * (expression list))
							 | WhileLoop of (string * expressionBlock)
							 | ComparisonOperation of (expression * comparisonOperator * expression)
							 | CastExpression of (string * string)
							 | InstanceOfExpression of (string * string) 

type classField = (valueType * string)

type methodParameter = (valueType * string)

type classMethod = (valueType * string * (methodParameter) list * expressionBlock) 

type classDefinition = (string * string * (classField) list * (classMethod) list)

type program = classDefinition list


(* Type checker *)

(**
 * Stack for keeping field-to-type mappings. The same variable name may appear more than one time in the stack.
 *)
type typeStack = (string * valueType) list

(**
 * Type information related to a method (methodName * returnType * parameterTypeList).
 *)
type methodSignature = (string * valueType * (valueType list))

(**
 * Type information related to a class (className * superClassName * fieldTypes * methodSignatureList).
 *)
type classDescriptor = (string * string * typeStack * (methodSignature list))

(**
 * The whole type environment, consisting of the variable type stack and the class descriptors (class type info).
 *)
type typeEnvironment = (typeStack * (classDescriptor list))

let rootClass = "Object"

(**
 * Pushes a given value with the given key into the variable type stack of the type environment.
 *)
let pushTypeIntoEnvStack (key : string) (value : valueType) (env : typeEnvironment) : typeEnvironment = 
	let (stack, classes) = env in 
		((key, value) :: stack, classes)

(**
 * Finds the type of the first variable from the type stack with the given name (key). A list is returned to allow 
 * "empty"  results (empty list indicates that nothing was found; singleton list indicates that at least one variable 
 * with the given name exists).
 *)
let rec findFirstInStack (key : string) (stack : typeStack) : valueType list = 
	match stack with 
  | [] -> []
  | (k, value) :: tail -> if k = key then [value] else findFirstInStack key tail

(**
 * Shorthand for searching in the type stack of an environment. @see findFirstInStack
 *)
let rec findFirstInEnvStack (key : string) (env : typeEnvironment) : valueType list = 
	let (stack, classes) = env in 
		findFirstInStack key stack

(**
 * Finds the class descriptor (from the environment) for the class with the given name. The same list-based optional
 * return value mechanism is used as in findFirstInStack. @see findFirstInStack
 *)
let rec findClassDescriptor (name : string) (env : typeEnvironment) : classDescriptor list =
	let (stack, classes) = env in 
		match classes with
		| [] -> []
		| (n, _, _, _) as head :: tail -> if n = name then [head] else findClassDescriptor name (stack, tail)

(**
 * Finds the class descriptor (from the environment) for the class with the given name. If the class does not exist
 * inside the environment, a Failure is raised.
 *)
let rec findClassDescriptorStrict (name : string) (env : typeEnvironment) : classDescriptor =
	match (findClassDescriptor name env) with
	| [cls] -> cls
	| _ -> raise (Failure("Unknown class " ^ name))

(**
 * Checks the ancestor classes of a given class for inheritance cycles. If a cycle is detected, a Failure is raised.
 *)
let rec checkClassInheritanceForCycles (name : string) (sub: string list) (env : typeEnvironment) : unit =
	if name = rootClass then
		()
	else if exists (fun c -> c = name) sub then
		raise (Failure("Class " ^ name ^ " is part of an inheritance cycle"))
	else
		let (_, super, _, _) = findClassDescriptorStrict name env in
			checkClassInheritanceForCycles super (name :: sub) env

(**
 * Checks if a given class (name) exists inside the type environment. 
 *)
let rec classExists (name : string) (env : typeEnvironment) : bool =
	match (findClassDescriptor name env) with
	| [] -> false
	| _ -> true

(**
 * Helper function which adds the given class' fields in the given field list (at the beginning).
 *)
let rec prependFields (lst : typeStack) (className : string) (env : typeEnvironment) : typeStack =
	if className = rootClass then
		lst
	else
		let (_, superClassName, fields, _) = findClassDescriptorStrict className env in
			prependFields (fields @ lst) superClassName env

(**
 * Retrieves all the field definitions for a given class (the fields are ordered by inheritance hierarchy position - depth).
 *)
let findAllFields (className : string) (env : typeEnvironment) : typeStack =
	prependFields [] className env

(**
 * Retrieves the types of the constructor parameters of a class (based on the class' field list).
 *)
let findConstructorParamTypes (className : string) (env : typeEnvironment) : valueType list =
	map (fun (pName, pType) -> pType) (prependFields [] className env)

(**
 * Checks if the given ancestor class name is actually an ancestor of (or identical to) the given class name.
 *)
let rec isAncestorOrSelf (className : string) (ancestor : string) (env : typeEnvironment) : bool = 
	className = ancestor || ancestor = rootClass || (className <> rootClass && (
		let (_, superClassName, _, _) = findClassDescriptorStrict className env in
			isAncestorOrSelf superClassName ancestor env
	))
(**
 * Determines if the source type may be assigned to the target type.
 *)
let isAssignable (source : valueType) (target : valueType) (env : typeEnvironment) : bool = 
	match (source, target) with
  | (BooleanType, BooleanType) -> true
  | (IntegerType, IntegerType) -> true
	| (VoidType, VoidType) -> true
  | ((FloatType | IntegerType), FloatType) -> true
	| (ClassName(c1), ClassName(c2)) -> isAncestorOrSelf c1 c2 env
	| (BottomType, (ClassName(_) | BottomType | VoidType)) -> true
  | _ -> false

(**
 * Determines if a method (given by signature) is compatible with the given name and actual parameter types.
 *)
let methodMatches (meth : methodSignature) (name : string) (actualParams : valueType list) (env : typeEnvironment) : bool =
	let (n, _, formalParams) = meth in
		n = name && (length actualParams) = (length formalParams) && fold_left (fun a b -> a && b) true 
			(map2 (fun actual formal -> isAssignable actual formal env) actualParams formalParams)

(**
 * Finds a matching method signature satisfying the following criteria: the method belongs to the given class and has a
 * signature compatible with the given name and actual parameter types. If no such method is found, a Failure is raised.
 * @see methodMatches
 *)
let rec findMatchingMethodStrict (className : string) (methodName : string) (actualParamTypes : valueType list) (env : typeEnvironment) : methodSignature =
	if className = rootClass then
		raise (Failure("Unknown method (with compatible signature) " ^ methodName))
	else
  	let (_, superClassName, _, methodList) = findClassDescriptorStrict className env in
  		try find (fun m -> methodMatches m methodName actualParamTypes env) methodList 
  		with Not_found -> findMatchingMethodStrict superClassName methodName actualParamTypes env

(**
 * Finds the type corresponding to the given field name in the given class.
 *)
let rec findFieldTypeStrict (className : string) (fieldName : string) (env : typeEnvironment) : valueType = 
	if className = rootClass then
		raise (Failure("Unknown field " ^ fieldName))
	else
		let (_, superClassName, fields, _) = findClassDescriptorStrict className env in
			match (findFirstInStack fieldName fields) with
			| [typ] -> typ
			| _ -> findFieldTypeStrict superClassName fieldName env

(**
 * Extracts the type of the given value expression.
 *)
let checkValue (v : value) (env : typeEnvironment) : valueType = 
	match v with 
  	| NullValue -> BottomType
  	| IntegerValue(_) -> IntegerType
  	| FloatValue(_) -> FloatType
  	| BooleanValue(_) -> BooleanType
  	| VoidValue -> VoidType

(**
 * Extracts the result type and checks type sound-ness of the operands of an binary expression.
 * @param e1 Left-hand side operand.
 * @param e2 Right-hand side operand.
 * @param typ The expected value type of the result (e.g. IntegerType for integer based operations).
 *)
let rec checkOperation (e1 : expression) (e2 : expression) (typ : valueType) (typeEnv : typeEnvironment) : valueType = 
	if ((isAssignable (checkExpression e1 typeEnv) typ typeEnv) && (isAssignable (checkExpression e2 typeEnv) typ typeEnv))
	then typ
	else raise (Failure("Incompatible types"))

(**
 * Checks the type sound-ness and extracts the result type from an expression block.
 *)
and checkBlock (b : expressionBlock) (typeEnv : typeEnvironment) : valueType = 
	match b with
  | ImmediateBlock(e) -> checkExpression e typeEnv
  | LocalVariableBlock(typ, var, e) -> checkExpression e (pushTypeIntoEnvStack var typ typeEnv)

(**
 * Checks that the given variable exists and returns its type.
 *)
and checkVariable (var : string) (typeEnv : typeEnvironment) : valueType = 
	match (findFirstInEnvStack var typeEnv) with
  | [] -> raise (Failure("Unknown variable " ^ var))
  | typ :: _ -> typ

(**
 * Checks the type sound-ness and extracts the result type from an assignment.
 * @param var The target variable (to be assigned).
 * @param e The expression whose result will be assigned to the variable.
 *)
and checkAssignment (var : string) (e : expression) (typeEnv : typeEnvironment) : valueType = 
	let typ = (checkVariable var typeEnv) in
		if isAssignable (checkExpression e typeEnv) typ typeEnv then 
			typ 
		else 
			raise (Failure("Incompatible types"))

(**
 * Checks if the given variable exists and is of a type which can be used with the instance of operator (reference type).
 *)
and checkInstanceOf (var : string) (clsName : string) (typeEnv : typeEnvironment) : valueType = 
	if clsName = rootClass || (classExists clsName typeEnv) then
  	match (checkVariable var typeEnv) with
    | ClassName(_) -> BooleanType
    | BottomType -> BooleanType
    | _ -> raise (Failure("Instance of may only be used on reference types"))
	else 
		raise (Failure("Unknown class " ^ clsName))

(**
 * Checks an if-then-else expression.
 * @param var The condition variable.
 * @param e1 The IF branch expression block.
 * @param e2 The ELSE branch expression block.
 *)
and checkIfThenElse (var : string) (e1 : expressionBlock) (e2 : expressionBlock) (typeEnv : typeEnvironment) : valueType = 
	let typ = (checkVariable var typeEnv) in
  	if isAssignable typ BooleanType typeEnv then 
  		(ignore (checkBlock e1 typeEnv); ignore (checkBlock e2 typeEnv); VoidType)
  	else
  		raise (Failure("Wrong if-then-else block condition type"))

(**
 * Checks an while-loop expression.
 * @param var The condition variable.
 * @param e The while loop body.
 *)
and checkWhileLoop (var : string) (e : expressionBlock) (typeEnv : typeEnvironment) : valueType = 
	let typ = (checkVariable var typeEnv) in
  	if isAssignable typ BooleanType typeEnv then 
  		(ignore (checkBlock e typeEnv); VoidType)
  	else
  		raise (Failure("Wrong while loop condition type"))
(**
 * Checks a comparison operation. All operators are allowed on numeric types, whilst the equality and non-equality
 * operators may also be used with reference and boolean types.
 * @param e1 The left-hand side expression of the comparison.
 * @param op The comparison operator.
 * @param e2 The right-hand side expression of the comparison.
 *)
and checkComparison (e1 : expression) (op : comparisonOperator) (e2 : expression) (typeEnv : typeEnvironment) : valueType = 
	let typ1 = (checkExpression e1 typeEnv) in
	let typ2 = (checkExpression e2 typeEnv) in
  	match (typ1, typ2, op) with
  	| ((BottomType | ClassName(_)), (BottomType | ClassName(_)), (EqualTo | NotEqualTo)) -> BooleanType
  	| (BooleanType, BooleanType, (EqualTo | NotEqualTo)) -> BooleanType
  	| ((IntegerType | FloatType), (IntegerType | FloatType), _) -> BooleanType
  	| _ -> raise (Failure("Unable to perform comparison between incompatible types"))

(**
 * Checks a sequence of expressions (an expression list). The resulting type is that of the last expression in the list.
 *)
and checkSequence (expList: expression list) (typeEnv : typeEnvironment) : valueType = 
	match expList with
  | [] -> VoidType
  | [e] -> checkExpression e typeEnv
  | e :: tail -> (ignore (checkExpression e typeEnv); checkSequence tail typeEnv)

(**
 * Checks a unary logical operator (e.g. 'not').
 *)
and checkUnaryLogicalOperation (e : expression) (typeEnv : typeEnvironment) : valueType =
	if (isAssignable (checkExpression e typeEnv) BooleanType typeEnv) then 
		BooleanType 
	else 
		raise (Failure("Incompatible types"))

(**
 * Checks an object field access, by checking if the given object exist, if it has a field with the given name and then 
 * returning the field's type.
 * @param var The variable holding the object whose field should be accessed. 
 * @param field The name of the field to be read.
 *)
and checkObjectField (var : string) (field : string) (typeEnv : typeEnvironment) : valueType = 
	match (checkVariable var typeEnv) with
	| ClassName(cls) -> findFieldTypeStrict cls field typeEnv
	| _ -> raise (Failure("Cannot access field of non-reference type"))

(**
 * Checks an object field assignment. The same rules apply as for @see checkObjectField, but additionally the given 
 * expression is evaluated and a type-compatibility check is done.
 * @param var The variable holding the object whose field should be accessed. 
 * @param field The name of the field to be written.
 * @param e The expression which will be evaluated to overwrite the field's value.
 *)
and checkFieldAssignment (var : string) (field : string) (e : expression) (typeEnv : typeEnvironment) : valueType = 
	match (checkVariable var typeEnv) with
	| ClassName(cls) -> let fieldType = findFieldTypeStrict cls field typeEnv in
		if (isAssignable (checkExpression e typeEnv) fieldType typeEnv) then 
			fieldType
		else
			raise (Failure("Incompatible types"))
	| _ -> raise (Failure("Cannot access field of non-reference type"))

(**
 * Checks a given cast operation (on a variable). The method also check if the classes exist (both the one given as
 * parameter and the one of the variable).
 *)
and checkCastOperation (className : string) (var : string) (typeEnv : typeEnvironment) : valueType =
	match (checkVariable var typeEnv) with
	| ClassName(cls) -> (
		ignore(findClassDescriptorStrict className typeEnv);
		ignore(findClassDescriptorStrict cls typeEnv);
		ClassName(className))
	| _ -> raise (Failure("Cannot cast non-reference type"))

(**
 * Type-checks a list of expressions and returns a list of the resulting types.
 *)
and checkExpressionList (expList : expression list) (typeEnv : typeEnvironment) : valueType list =
	map (fun e -> checkExpression e typeEnv) expList

(**
 * Type-checks an object instantiation operation (checks if the class exists, extracts its constructor parameter
 * types and checks these types against the given expressions).
 * @param className The name of the class which is instantiated. 
 * @param expList A list of expressions passed to the class' constructor.
 *)
and checkObjectInstantiation (className : string) (expList : expression list) (typeEnv : typeEnvironment) : valueType =
	let actualParamTypes = checkExpressionList expList typeEnv in
	let formalParamTypes = findConstructorParamTypes className typeEnv in
	if (length actualParamTypes) = (length formalParamTypes) then
		(
			iter2 (fun actual formal -> if (isAssignable actual formal typeEnv) = false then 
				raise (Failure("Incompatible types"))) actualParamTypes formalParamTypes;
			ClassName(className)
		)
	else
		raise (Failure("Invalid number of arguments for constructor"))
(**
 * Type-checks an method call (checks if the variable exists, the method exists, extracts the method's parameter
 * types and checks these types against the given expressions).
 * @param var The name of the variable holding the object.
 * @param methodName The name of the method to be called.
 * @param expList A list of expressions passed as the method's parameters.
 *)
and checkMethodCall (var : string) (methodName : string) (expList : expression list) (typeEnv : typeEnvironment) : valueType =
	let actualParamTypes = checkExpressionList expList typeEnv in
		match (checkVariable var typeEnv) with
  	| ClassName(cls) -> let (_, retType, _) = findMatchingMethodStrict cls methodName actualParamTypes typeEnv in retType
  	| _ -> raise (Failure("Cannot call method on non-reference type"))

(**
 * Checks an arbitrary expression for type sound-ness.
 *)
and checkExpression (e : expression) (typeEnv : typeEnvironment) : valueType = 
	match e with
    | Value(v) -> checkValue v typeEnv
  	| Variable(v) -> checkVariable v typeEnv
  	| IntegerOperation(e1, _, e2) -> checkOperation e1 e2 IntegerType typeEnv
  	| FloatOperation(e1, _, e2) -> checkOperation e1 e2 FloatType typeEnv
  	| LogicalBinaryOperation(e1, _, e2) -> checkOperation e1 e2 BooleanType typeEnv
  	| LogicalUnaryOperation(_, e) -> checkUnaryLogicalOperation e typeEnv
  	| ExpressionBlock(b) -> checkBlock b typeEnv
  	| InstanceOfExpression(var, clsName) -> checkInstanceOf var clsName typeEnv
  	| Sequence(expList) -> checkSequence expList typeEnv
  	| WhileLoop(var, e) -> checkWhileLoop var e typeEnv
  	| IfThenElse(var, e1, e2) -> checkIfThenElse var e1 e2 typeEnv
  	| ComparisonOperation(e1, op, e2) -> checkComparison e1 op e2 typeEnv
  	| VariableAssignment(var, e) -> checkAssignment var e typeEnv
		| ObjectFieldAssignment(var, field, e) -> checkFieldAssignment var field e typeEnv
		| ObjectField(var, field) -> checkObjectField var field typeEnv
		| CastExpression(cls, var) -> checkCastOperation cls var typeEnv
		| ObjectInstantiation(cls, expList) -> checkObjectInstantiation cls expList typeEnv
		| MethodCall(var, meth, expList) -> checkMethodCall var meth expList typeEnv

(**
 * Type-checks a method definition (by checking for duplicate parameters, type-checking the method's body and
 * ensuring that the actual method body return type is compatible with the declared return type.
 *)
let checkMethod (meth : classMethod) (env : typeEnvironment) : unit = 
	let (formalReturnType, _, params, block) = meth in
	let (stack, classes) = env in
	let extendedEnv = ((map (fun (varTyp, varNam) -> (varNam, varTyp)) params) @ stack, classes) in
  	ignore(
  		fold_left (fun a e -> if a <> "" && a = e then raise (Failure ("Duplicate parameter name " ^ e)) else e) "" 
  			(sort compare (map (fun (_, varNam) -> varNam) params))
  	);
		let actualReturnType = checkBlock block extendedEnv in
		(* Included void type check to allow "discarding" the result of a method (same as the OCaml ignore function) *)
		if formalReturnType <> VoidType && not (isAssignable actualReturnType formalReturnType extendedEnv) then
			raise (Failure("Incompatible types"))
(**
 * Type-checks a whole class definition (by checking for duplicate fields and type-checking each method). This
 * function also adds the 'this' pseudo-variable to the type context of each method (when performing the type-check).
 *)
let checkClass (cls : classDefinition) (env : typeEnvironment) : unit = 
	let (name, super, _, methods) = cls in
	let (stack, classes) = env in
		checkClassInheritanceForCycles name [] env;
  	let allFields = findAllFields name env in
  		ignore(
  			fold_left (fun a e -> if a <> "" && a = e then raise (Failure ("Duplicate field name " ^ e)) else e) "" 
    			(sort compare (map (fun (varNam, _) -> varNam) allFields))
  		);
  		let extendedEnv = ((("this", ClassName(name)) :: stack), classes) in
  			iter (fun m -> checkMethod m extendedEnv) methods	

let fieldDefinitionToTypeStackElement (field : classField) : (string * valueType) =
	let (typ, name) = field in 
		(name, typ)

let methodDefinitionToMethodSignature (meth : classMethod) : methodSignature =
	let (retType, name, params, _) = meth in
		(name, retType, (map (fun (typ, _) -> typ) params))

let classDefinitionToDescriptor (cls : classDefinition) : classDescriptor =
	let (name, super, fields, methods) = cls in
		(name, super, (map fieldDefinitionToTypeStackElement fields), (map methodDefinitionToMethodSignature methods))				

(** 
 * Transforms a program AST into a list of class descriptors (type info).
 *)
let extractClassDescriptorsFromProgram (p : program) : classDescriptor list =
	map classDefinitionToDescriptor p

(**
 * Type-checks a whole program (by deriving the type environment, checking for duplicate classes, searching for the 
 * main method and then type-checking each individual class).
 *)
let checkProgram (p : program) : unit =
	let classes = extractClassDescriptorsFromProgram p in
	let env = ([], classes) in
		ignore(
			fold_left (fun a e -> if a <> "" && a = e then raise (Failure ("Duplicate class name " ^ e)) else e) "" 
  			(sort compare (map (fun (className, _, _, _) -> className) classes))
		);
		if length classes > 0 then
		(
			ignore(
				let (lastClassName, _, _, _) = hd (rev classes) in
					try findMatchingMethodStrict lastClassName "main" [] env with Failure(_) -> raise (Failure("Unable to find main method"))
			);
			iter (fun c -> checkClass c env) p	
		)
		else
			raise (Failure ("Empty program"))


