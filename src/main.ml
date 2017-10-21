type primitiveType = IntegerType 
								 	 | FloatType 
									 | BooleanType 
									 | VoidType

type valueType = PrimitiveType of primitiveType 
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
							  
