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
							 | Sequence of (expression * expression)
							 | IfThenElse of (string * expressionBlock * expressionBlock)
							 | IntegerOperation of (expression * integerOperator * expression)
							 | FloatOperation of (expression * floatOperator * expression)
							 | LogicalBinaryOperation of (expression * binaryLogicalOperator * expression)
							 | LogicalUnaryOperation of (unaryLogicalOperator * expression)
							 | ObjectInstantiation of (string * (string) list)
							 | MethodCall of (string * string * (string) list)
							 | WhileLoop of (string * expressionBlock)
							 | ComparisonOperation of (expression * comparisonOperator * expression)
							 | CastExpression of (string * string)
							 | InstanceOfExpression of (string * string) 

type classField = (valueType * string)

type methodParameter = (valueType * string)

type classMethod = (valueType * string * (methodParameter) list * expressionBlock) 

type classDefinition = (string * string * (classField) list * (classMethod) list)

type program = (string * classDefinition) list

(* Type checker *)

exception TypeError of string

let isAssignable source target _ = match target with
| BooleanType -> source == BooleanType
| IntegerType -> source == IntegerType
| FloatType -> source == IntegerType || source == FloatType
| _ -> false

let checkValue v _ = match v with 
	| NullValue -> BottomType
	| IntegerValue(_) -> IntegerType
	| FloatValue(_) -> FloatType
	| BooleanValue(_) -> BooleanType
	| VoidValue -> VoidType
	| _ -> raise (TypeError("Unknown value type"))

let rec checkOperation e1 e2 typ typeEnv = 
	if ((isAssignable (checkExpression e1 typeEnv) typ typeEnv) && (isAssignable (checkExpression e2 typeEnv) typ typeEnv))
	then typ
	else raise (TypeError("Incompatible types"))

and checkExpression e typeEnv = match e with
  | Value(v) -> checkValue v typeEnv
	| IntegerOperation(e1, _, e2) -> checkOperation e1 e2 IntegerType typeEnv
	| FloatOperation(e1, _, e2) -> checkOperation e1 e2 FloatType typeEnv
	| LogicalBinaryOperation(e1, _, e2) -> checkOperation e1 e2 BooleanType typeEnv
	| LogicalUnaryOperation(_, e) -> if (isAssignable (checkExpression e typeEnv) BooleanType typeEnv) then BooleanType else raise (TypeError("Incompatible types"))
  | _ -> raise (TypeError("Unknown expression type"))







							  
