open OUnit2
open Main

(* Is-assignable check *)
let booleanIsAssignableToBoolean test_ctxt = assert_equal true 
	(isAssignable BooleanType BooleanType ([], []))
let integerIsAssignableToInteger test_ctxt = assert_equal true 
	(isAssignable IntegerType IntegerType ([], []))
let floatIsAssignableToFloat test_ctxt = assert_equal true 
	(isAssignable FloatType FloatType ([], []))
let integerIsAssignableToFloat test_ctxt = assert_equal true 
	(isAssignable IntegerType FloatType ([], []))
let nullIsAssignableToNull test_ctxt = assert_equal true 
	(isAssignable BottomType BottomType ([], []))
let classAIsAssignableToClassA test_ctxt = assert_equal true 
	(isAssignable (ClassName("A")) (ClassName("A")) ([], [("A", "Object", [], [])]))
let classIsAssignableToSuperClass test_ctxt = assert_equal true 
	(isAssignable (ClassName("A")) (ClassName("B")) ([], [("A", "B", [], []); ("B", "Object", [], [])]))
let classIsAssignableToObject test_ctxt = assert_equal true 
	(isAssignable (ClassName("A")) (ClassName("Object")) ([], [("A", "B", [], []); ("B", "Object", [], [])]))
let booleanIsNotAssignableToInteger test_ctxt = assert_equal false 
	(isAssignable BooleanType IntegerType ([], []))
let integerIsNotAssignableToBoolean test_ctxt = assert_equal false 
	(isAssignable IntegerType BooleanType ([], []))
let floatIsNotAssignableToInteger test_ctxt = assert_equal false 
	(isAssignable FloatType IntegerType ([], []))
let nullIsNotAssignableToInteger test_ctxt = assert_equal false 
	(isAssignable BottomType IntegerType ([], []))
let unrelatedClassesAreNotAssignable test_ctxt = assert_equal false 
	(isAssignable (ClassName("A")) (ClassName("B")) ([], [("A", "Object", [], []); ("B", "Object", [], [])]))

(* Value expression *)
let nullValueReturnsBottomType test_ctxt = assert_equal BottomType 
	(checkExpression (Value(NullValue)) ([], []))
let simpleValueReturnsRightType test_ctxt = assert_equal IntegerType 
	(checkExpression (Value(IntegerValue(1))) ([], []))
	
(* Variable expression *)
let simpleVariableWithSingletonEnvironment test_ctxt = assert_equal IntegerType 
	(checkExpression (Variable("a")) ([("a", IntegerType)], []))
let simpleVariableWithLongEnvironment test_ctxt = assert_equal BooleanType 
	(checkExpression (Variable("a")) ([("c", BooleanType); ("b", FloatType); ("a", BooleanType); ("a", IntegerType)], []))
let simpleVariableWithUnknownVar test_ctxt = assert_raises (Failure "Unknown variable b")
	(fun() -> checkExpression (Variable("b")) ([("a", IntegerType)], []))

(* Variable assignment *)
let simpleVariableAssignment test_ctxt = assert_equal IntegerType 
	(checkExpression (VariableAssignment("a", Value(IntegerValue(1)))) ([("a", IntegerType)], []))
let simpleVariableAssignmentWithIncompatibleTypes test_ctxt = assert_equal BooleanType 
	(checkExpression (VariableAssignment("a", Value(BooleanValue(false)))) ([("a", IntegerType)], []))
let simpleVariableAssignmentWithUnknownVar test_ctxt = assert_raises (Failure "Unknown variable b")
	(fun() -> checkExpression (VariableAssignment("b", Value(IntegerValue(1)))) ([("a", IntegerType)], []))

(* Integer operation expression *)
let simpleIntegerOperationWithBothOperandsInteger test_ctxt = assert_equal IntegerType
	(checkExpression (IntegerOperation(Value(IntegerValue(1)), IntegerAddition, Value(IntegerValue(2)))) ([], []))
let complexIntegerOperationWithBothOperandsInteger test_ctxt = assert_equal IntegerType
	(checkExpression (IntegerOperation(
			IntegerOperation(Value(IntegerValue(1)), IntegerAddition, Value(IntegerValue(2))), 
			IntegerAddition, 
			IntegerOperation(Value(IntegerValue(1)), IntegerAddition, Value(IntegerValue(2)))
	)) ([], []))
let simpleIntegerOperationWithFloatOperand test_ctxt = assert_raises (Failure "Incompatible types")
	(fun() -> checkExpression (IntegerOperation(Value(IntegerValue(1)), IntegerAddition, Value(FloatValue(2.0)))) ([], []))
let simpleIntegerOperationWithClassOperand test_ctxt = assert_raises (Failure "Incompatible types")
	(fun() -> checkExpression (IntegerOperation(Value(IntegerValue(1)), IntegerAddition, Variable("a"))) ([("a", ClassName("A"))], []))
	
(* Float operation expression *)
let simpleFloatOperationWithBothOperandsFloat test_ctxt = assert_equal FloatType
	(checkExpression (FloatOperation(Value(FloatValue(1.0)), FloatAddition, Value(FloatValue(2.0)))) ([], []))
let complexFloatOperationWithBothOperandsFloat test_ctxt = assert_equal FloatType
	(checkExpression (FloatOperation(
			FloatOperation(Value(FloatValue(1.0)), FloatAddition, Value(FloatValue(2.0))), 
			FloatAddition, 
			FloatOperation(Value(FloatValue(1.0)), FloatAddition, Value(FloatValue(2.0)))
	)) ([], []))
let simpleFloatOperationWithOneOperandInteger test_ctxt = assert_equal FloatType
	(checkExpression (FloatOperation(Value(FloatValue(1.0)), FloatAddition, Value(IntegerValue(1)))) ([], []))
let simpleFloatOperationWithClassOperand test_ctxt = assert_raises (Failure "Incompatible types")
	(fun() -> checkExpression (FloatOperation(Value(FloatValue(1.0)), FloatAddition, Variable("a"))) ([("a", ClassName("A"))], []))

(* Logical binary operation expression *)
let simpleBooleanOperationWithBothOperandsBoolean test_ctxt = assert_equal BooleanType
	(checkExpression (LogicalBinaryOperation(Value(BooleanValue(true)), LogicalAnd, Value(BooleanValue(false)))) ([], []))
let complexBooleanOperationWithBothOperandsBoolean test_ctxt = assert_equal BooleanType
	(checkExpression (LogicalBinaryOperation(
		LogicalBinaryOperation(Value(BooleanValue(true)), LogicalAnd, Value(BooleanValue(false))), 
		LogicalOr, 
		LogicalBinaryOperation(Value(BooleanValue(true)), LogicalAnd, Value(BooleanValue(false)))
	)) ([], []))
let simpleBooleanOperationWithFloatOperand test_ctxt = assert_raises (Failure "Incompatible types")
	(fun() -> checkExpression (LogicalBinaryOperation(Value(BooleanValue(true)), LogicalAnd, Value(FloatValue(1.0)))) ([], []))

(* Logical unary operation expression *)
let simpleUnaryBooleanOperationWithBooleanOperand test_ctxt = assert_equal BooleanType
	(checkExpression (LogicalUnaryOperation(LogicalNot, Value(BooleanValue(false)))) ([], []))
let simpleUnaryBooleanOperationWithClassOperand test_ctxt = assert_raises (Failure "Incompatible types")
	(fun() -> checkExpression (LogicalUnaryOperation(LogicalNot, Variable("a"))) ([("a", ClassName("A"))], []))

(* Expression block *)
let simpleImmediateBlockReturnsInnerType test_ctxt = assert_equal IntegerType 
	(checkExpression (ExpressionBlock(ImmediateBlock(Value(IntegerValue(1))))) ([], []))
let simpleImmediateBlockPropagatesTypeEnv test_ctxt = assert_equal IntegerType 
	(checkExpression (ExpressionBlock(ImmediateBlock(Variable("a")))) ([("a", IntegerType)], []))
let simpleImmediateBlockPropagatesTypeError test_ctxt = assert_raises (Failure "Unknown variable b")
	(fun() -> checkExpression (ExpressionBlock(ImmediateBlock(Variable("b")))) ([("a", IntegerType)], []))
let simpleLocalBlockWithDirectVariableRead test_ctxt = assert_equal IntegerType 
	(checkExpression (ExpressionBlock(LocalVariableBlock(IntegerType, "a", Variable("a")))) ([], []))

(* Instance-of expression *)
let instanceOfCheckWithClassVariable test_ctxt = assert_equal BooleanType
	(checkExpression (InstanceOfExpression("a", "A")) ([("a", ClassName("A"))], [("A", "Object", [], [])]))
let instanceOfCheckWithNullVariable test_ctxt = assert_equal BooleanType
	(checkExpression (InstanceOfExpression("a", "A")) ([("a", BottomType)], [("A", "Object", [], [])]))
let instanceOfCheckWithClassVariableAndObjectCheck test_ctxt = assert_equal BooleanType
	(checkExpression (InstanceOfExpression("a", "Object")) ([("a", ClassName("A"))], []))
let instanceOfCheckWithUnknownClass test_ctxt = assert_raises (Failure "Unknown class A")
	(fun() -> checkExpression (InstanceOfExpression("a", "A")) ([("a", ClassName("A"))], []))

(* Sequence of expressions *)
let singleElementSequencePropagatesResult test_ctxt = assert_equal IntegerType 
	(checkExpression (Sequence([Value(IntegerValue(1))])) ([], []))
let multipleElementSequencePropagatesLastResult test_ctxt = assert_equal IntegerType 
	(checkExpression (Sequence([Value(FloatValue(1.0)); Value(BooleanValue(false)); Value(IntegerValue(1))])) ([], []))
let multipleElementSequencePropagatesErrorWhenInner test_ctxt = assert_raises (Failure "Unknown variable a")
	(fun() -> checkExpression (Sequence([Variable("a"); Value(BooleanValue(false)); Value(IntegerValue(1))])) ([], []))
let multipleElementSequencePropagatesErrorWhenLast test_ctxt = assert_raises (Failure "Unknown variable a")
	(fun() -> checkExpression (Sequence([Value(BooleanValue(false)); Value(IntegerValue(1)); Variable("a")])) ([], []))

(* While loop *)
let whileLoopWithSimpleInnerBlock test_ctxt = assert_equal VoidType 
	(checkExpression (WhileLoop("a", ImmediateBlock(Value(IntegerValue(1))))) ([("a", BooleanType)], []))
let whileLoopWithWrongVariableType test_ctxt = assert_raises (Failure "Wrong while loop condition type") 
	(fun() -> checkExpression (WhileLoop("a", ImmediateBlock(Value(IntegerValue(1))))) ([("a", IntegerType)], []))
let whileLoopWithUnknownVariable test_ctxt = assert_raises (Failure "Unknown variable c") 
	(fun() -> checkExpression (WhileLoop("c", ImmediateBlock(Value(IntegerValue(1))))) ([("a", BooleanType)], []))

(* If-then-else block *)
let ifThenElseWithSimpleInnerBlocks test_ctxt = assert_equal VoidType 
	(checkExpression (IfThenElse("a", ImmediateBlock(Value(IntegerValue(1))), ImmediateBlock(Value(IntegerValue(1))))) ([("a", BooleanType)], []))
let ifThenElseWithWrongVariableType test_ctxt = assert_raises (Failure "Wrong if-then-else block condition type") 
	(fun() -> checkExpression (IfThenElse("a", ImmediateBlock(Value(IntegerValue(1))), ImmediateBlock(Value(IntegerValue(1))))) ([("a", IntegerType)], []))
let ifThenElseWithUnknownVariable test_ctxt = assert_raises (Failure "Unknown variable c") 
	(fun() -> checkExpression (IfThenElse("c", ImmediateBlock(Value(IntegerValue(1))), ImmediateBlock(Value(IntegerValue(1))))) ([("a", IntegerType)], []))

(* Comparison operation *)
let comparisonOperationWithBothOperandsFloat test_ctxt = assert_equal BooleanType
	(checkExpression (ComparisonOperation(Value(FloatValue(1.0)), LowerThan, Value(FloatValue(2.0)))) ([], []))
let comparisonOperationWithFloatAndIntegerOperands test_ctxt = assert_equal BooleanType
	(checkExpression (ComparisonOperation(Value(FloatValue(1.0)), LowerThan, Value(IntegerValue(2)))) ([], []))
let equalityComparisonOperationWithBothOperandsBoolean test_ctxt = assert_equal BooleanType
	(checkExpression (ComparisonOperation(Value(BooleanValue(true)), EqualTo, Value(BooleanValue(false)))) ([], []))
let lowerThanComparisonOperationWithBothOperandsBooleanRaisesError test_ctxt = assert_raises 
	(Failure "Unable to perform comparison between incompatible types")
	(fun() -> checkExpression (ComparisonOperation(Value(BooleanValue(true)), LowerThan, Value(BooleanValue(false)))) ([], []))
let equalityComparisonOperationWithBothOperandsClass test_ctxt = assert_equal BooleanType
	(checkExpression (ComparisonOperation(Variable("a"), EqualTo, Variable("a"))) ([("a", ClassName("A"))], []))
let equalityComparisonOperationWithClassAndNullOperand test_ctxt = assert_equal BooleanType
	(checkExpression (ComparisonOperation(Variable("a"), EqualTo, Value(NullValue))) ([("a", ClassName("A"))], []))
let lowerThanComparisonOperationWithBothOperandsClassRaisesErorr test_ctxt = assert_raises 
	(Failure "Unable to perform comparison between incompatible types")
	(fun() -> checkExpression (ComparisonOperation(Variable("a"), LowerThan, Variable("a"))) ([("a", ClassName("A"))], []))

(* Object field *)
let simpleFieldAssignment test_ctxt = assert_equal IntegerType 
	(checkExpression (ObjectField("a", "f")) ([("a", ClassName("A"))], [("A", "Object", [("f", IntegerType)], [])]))
let fieldAssignmentWithInheritance test_ctxt = assert_equal IntegerType 
	(checkExpression (ObjectField("a", "f")) ([("a", ClassName("A"))], [("A", "B", [], []); ("B", "Object", [("f", IntegerType)], [])]))
let simpleFieldAssignmentWithUnknownField test_ctxt = assert_raises (Failure "Unknown field x") 
	(fun() -> checkExpression (ObjectField("a", "x")) ([("a", ClassName("A"))], [("A", "Object", [("f", IntegerType)], [])]))
let simpleFieldAssignmentWithUnknownClass test_ctxt = assert_raises (Failure "Unknown class X") 
	(fun() -> checkExpression (ObjectField("a", "f")) ([("a", ClassName("X"))], [("A", "Object", [("f", IntegerType)], [])]))

(* Field assignment *)
let simpleFieldAssignment test_ctxt = assert_equal IntegerType 
	(checkExpression (ObjectFieldAssignment("a", "f", Value(IntegerValue(1)))) ([("a", ClassName("A"))], [("A", "Object", [("f", IntegerType)], [])]))
let fieldAssignmentWithInheritance test_ctxt = assert_equal IntegerType 
	(checkExpression 
		(ObjectFieldAssignment("a", "f", Value(IntegerValue(1)))) 
		([("a", ClassName("A"))], [("A", "B", [], []); ("B", "Object", [("f", IntegerType)], [])]))
let fieldAssignmentWithDeepInheritance test_ctxt = assert_equal IntegerType 
	(checkExpression 
		(ObjectFieldAssignment("a", "f", Value(IntegerValue(1)))) 
		([("a", ClassName("A"))], [("A", "Z", [], []); ("Z", "X", [], []); ("X", "B", [], []); ("B", "Object", [("f", IntegerType)], [])]))
let fieldAssignmentWithInheritanceAndFieldOverloading test_ctxt = assert_equal IntegerType 
	(checkExpression 
		(ObjectFieldAssignment("a", "f", Value(IntegerValue(1)))) 
		([("a", ClassName("A"))], [("A", "B", [("f", IntegerType)], []); ("B", "Object", [("f", BooleanType)], [])]))
let simpleFieldAssignmentWithUnknownField test_ctxt = assert_raises (Failure "Unknown field x") 
	(fun() -> checkExpression 
		(ObjectFieldAssignment("a", "x", Value(IntegerValue(1)))) 
		([("a", ClassName("A"))], [("A", "Object", [("f", IntegerType)], [])]))
let simpleFieldAssignmentWithUnknownClass test_ctxt = assert_raises (Failure "Unknown class X") 
	(fun() -> checkExpression 
		(ObjectFieldAssignment("a", "f", Value(IntegerValue(1)))) 
		([("a", ClassName("X"))], [("A", "Object", [("f", IntegerType)], [])]))
let simpleFieldAssignmentWithWrongType test_ctxt = assert_raises (Failure "Incompatible types") 
	(fun() -> checkExpression 
		(ObjectFieldAssignment("a", "f", Value(FloatValue(1.0)))) 
		([("a", ClassName("A"))], [("A", "Object", [("f", IntegerType)], [])]))

(* Class cast expression *)
let simpleClassCast test_ctxt = assert_equal (ClassName("B"))
	(checkExpression (CastExpression("B", "a")) ([("a", ClassName("A"))], [("A", "Z", [], []); ("B", "Z", [], [])]))
let classCastWithUnknownVar test_ctxt = assert_raises (Failure "Unknown variable b")
	(fun() -> checkExpression (CastExpression("B", "b")) ([("a", ClassName("A"))], [("A", "Z", [], []); ("B", "Z", [], [])]))
let classCastWithUnknownVarClass test_ctxt = assert_raises (Failure "Unknown class Y")
	(fun() -> checkExpression (CastExpression("B", "a")) ([("a", ClassName("Y"))], [("A", "Z", [], []); ("B", "Z", [], [])]))
let classCastWithUnknownTargetClass test_ctxt = assert_raises (Failure "Unknown class Y")
	(fun() -> checkExpression (CastExpression("Y", "a")) ([("a", ClassName("A"))], [("A", "Z", [], []); ("B", "Z", [], [])]))

(* Object instantiation *)
let noArgConstructorWithNoInheritance test_ctxt = assert_equal (ClassName("A"))
	(checkExpression (ObjectInstantiation("A", [])) ([], [("A", "Object", [], [])]))
let noArgConstructorWithDeepInheritance test_ctxt = assert_equal (ClassName("A"))
	(checkExpression (ObjectInstantiation("A", [])) ([], [("A", "B", [], []); ("B", "C", [], []); ("C", "Object", [], [])]))
let multiArgConstructorWithSameTypesAndNoInheritance test_ctxt = assert_equal (ClassName("A"))
	(checkExpression 
		(ObjectInstantiation("A", [Value(IntegerValue(1)); Value(FloatValue(1.0))])) 
		([], [("A", "Object", [("a", IntegerType); ("b", FloatType)], [])]))
let multiArgConstructorWithCompatibleTypesAndNoInheritance test_ctxt = assert_equal (ClassName("A"))
	(checkExpression 
		(ObjectInstantiation("A", [Value(IntegerValue(1)); Value(IntegerValue(1))])) 
		([], [("A", "Object", [("a", FloatType); ("b", FloatType)], [])]))
let multiArgConstructorWithSameTypesAndDeepInheritance test_ctxt = assert_equal (ClassName("A"))
	(checkExpression 
		(ObjectInstantiation("A", [Value(IntegerValue(1)); Value(FloatValue(1.0))])) 
		([], [("A", "B", [("b", FloatType)], []); ("B", "C", [], []); ("C", "Object", [("a", IntegerType)], [])]))
let tooFewArgConstructor test_ctxt = assert_raises (Failure "Invalid number of arguments for constructor")
	(fun() -> checkExpression (ObjectInstantiation("A", [])) ([], [("A", "Object", [("a", FloatType)], [])]))
let tooManyArgConstructor test_ctxt = assert_raises (Failure "Invalid number of arguments for constructor")
	(fun() -> checkExpression (ObjectInstantiation("A", [Value(IntegerValue(1))])) ([], [("A", "Object", [], [])]))
let unknownClassConstructor test_ctxt = assert_raises (Failure "Unknown class X")
	(fun() -> checkExpression (ObjectInstantiation("X", [])) ([], []))
let wrongArgumentTypeConstructorWithNoInheritance test_ctxt = assert_raises (Failure "Incompatible types")
	(fun() -> checkExpression 
		(ObjectInstantiation("A", [Value(FloatValue(1.0)); Value(FloatValue(1.0))])) 
		([], [("A", "Object", [("a", IntegerType); ("b", IntegerType)], [])]))
let wrongArgumentTypeConstructorWithDeepInheritance test_ctxt = assert_raises (Failure "Incompatible types")
	(fun() -> checkExpression 
		(ObjectInstantiation("A", [Value(FloatValue(1.0)); Value(FloatValue(1.0))])) 
		([], [("A", "B", [("b", IntegerType)], []); ("B", "C", [], []); ("C", "Object", [("a", IntegerType)], [])]))

(* Method call *)
let noArgMethodWithNoInheritance test_ctxt = assert_equal IntegerType
	(checkExpression (MethodCall("a", "m", [])) ([("a", ClassName("A"))], [("A", "Object", [], [("m", IntegerType, [])])]))
let noArgMethodWithDeepInheritance test_ctxt = assert_equal IntegerType
	(checkExpression (
		MethodCall("a", "m", [])) 
		([("a", ClassName("A"))], [("A", "B", [], []); ("B", "C", [], []); ("C", "Object", [], [("m", IntegerType, [])])]))
let multiArgMethodWithNoInheritance test_ctxt = assert_equal IntegerType
	(checkExpression 
		(MethodCall("a", "m", [Value(IntegerValue(1)); ObjectInstantiation("X", [])])) 
		([("a", ClassName("A"))], [("X", "Object", [], []); ("A", "Object", [], [("m", IntegerType, [IntegerType; ClassName("X")])])]))
let multiArgMethodWithDeepInheritance test_ctxt = assert_equal IntegerType
	(checkExpression 
		(MethodCall("a", "m", [Value(IntegerValue(1)); ObjectInstantiation("X", [])])) 
		([("a", ClassName("A"))], [("X", "Object", [], []); ("A", "B", [], []); ("B", "C", [], []); 
			("C", "Object", [], [("m", IntegerType, [IntegerType; ClassName("X")])])]))
let multiArgMethodWithSimpleOverloading test_ctxt = assert_equal IntegerType
	(checkExpression 
		(MethodCall("a", "m", [Value(IntegerValue(1)); ObjectInstantiation("X", [])])) 
		([("a", ClassName("A"))], [("X", "Object", [], []); 
			("A", "Object", [], [("m", BooleanType, [IntegerType]); ("m", IntegerType, [IntegerType; ClassName("X")])])]))
let multiArgMethodWithCompatibleTypes test_ctxt = assert_equal IntegerType
	(checkExpression 
		(MethodCall("a", "m", [Value(IntegerValue(1))])) 
		([("a", ClassName("A"))], [("A", "Object", [], [("m", IntegerType, [FloatType])])]))
let methodCallWithUnknownMethodName test_ctxt = assert_raises (Failure "Unknown method (with compatible signature) xm")
	(fun () -> checkExpression (MethodCall("a", "xm", [])) ([("a", ClassName("A"))], [("A", "Object", [], [("m", IntegerType, [])])]))
let methodCallWithMismatchingSignatureByNumber test_ctxt = assert_raises (Failure "Unknown method (with compatible signature) m")
	(fun () -> checkExpression (MethodCall("a", "m", [])) ([("a", ClassName("A"))], [("A", "Object", [], [("m", IntegerType, [IntegerType])])]))
let methodCallWithMismatchingSignatureByType test_ctxt = assert_raises (Failure "Unknown method (with compatible signature) m")
	(fun () -> checkExpression (
		MethodCall("a", "m", [Value(FloatValue(1.0))])) 
		([("a", ClassName("A"))], [("A", "Object", [], [("m", IntegerType, [IntegerType])])]))

(* Check method definition and content *)
let noArgsFullMethodWithIntegerReturnType test_ctxt = assert_equal ()
	(checkMethod (IntegerType, "method", [], (ImmediateBlock(Value(IntegerValue(1))))) ([], []))
let noArgsFullMethodWithIgnoreReturn test_ctxt = assert_equal ()
	(checkMethod (VoidType, "method", [], (ImmediateBlock(Value(IntegerValue(1))))) ([], []))
let noArgsFullMethodWithVoidReturnType test_ctxt = assert_equal ()
	(checkMethod (VoidType, "method", [], (ImmediateBlock(Value(NullValue)))) ([], []))
let multiArgsFullMethodWithIntegerReturnTypeAndParamUsage test_ctxt = assert_equal ()
	(checkMethod (IntegerType, "method", [(IntegerType, "a"); (FloatType, "b")], (ImmediateBlock(Variable("a")))) ([], []))
let multiArgsFullMethodWithWrongReturnType test_ctxt = assert_raises (Failure "Incompatible types")
	(fun () -> checkMethod (IntegerType, "method", [(IntegerType, "a"); (FloatType, "b")], (ImmediateBlock(Variable("b")))) ([], []))
let multiArgsFullMethodWithDuplicateParamName test_ctxt = assert_raises (Failure "Duplicate parameter name a")
	(fun () -> checkMethod (IntegerType, "method", [(IntegerType, "a"); (FloatType, "a")], (ImmediateBlock(Variable("a")))) ([], []))

(* Check class definition including methods *)
let noMethodNoFieldClassWithoutInheritance test_ctxt = assert_equal ()
	(checkClass ("A", "Object", [], []) ([], [("A", "Object", [], [])]))
let noMethodNoFieldClassWitDeepInheritance test_ctxt = assert_equal ()
	(checkClass ("A", "B", [], []) ([], [("A", "B", [], []); ("B", "C", [], []); ("C", "Object", [], [])]))
let noMethodMultiFieldClassWitDeepInheritance test_ctxt = assert_equal ()
	(checkClass ("A", "B", [], []) ([], [("A", "B", [("a", IntegerType)], []); ("B", "C", [("b", FloatType)], []); ("C", "Object", [], [])]))
let multiMethodMultiFieldClassWitDeepInheritance test_ctxt = assert_equal ()
	(checkClass 
		("A", "B", [(IntegerType, "a")], [
			(IntegerType, "m1", [(IntegerType, "p")], (ImmediateBlock(Variable("p"))));
			(FloatType, "m2", [(FloatType, "p")], (ImmediateBlock(Variable("p"))))
		]) 
		([], [
			("A", "B", [("a", IntegerType)], [("m1", IntegerType, [IntegerType]); ("m2", FloatType, [FloatType])]); 
			("B", "C", [("b", FloatType)], []); 
			("C", "Object", [], [])
		]))
let multiMethodMultiFieldClassWithDeepInheritanceAndThisUsage test_ctxt = assert_equal ()
	(checkClass 
		("A", "B", [(IntegerType, "a")], [
			(IntegerType, "m1", [(IntegerType, "p")], (ImmediateBlock(ObjectField("this", "a"))));
			(FloatType, "m2", [(FloatType, "p")], (ImmediateBlock(ObjectFieldAssignment("this", "b", Variable("p")))))
		]) 
		([], [
			("A", "B", [("a", IntegerType)], [("m1", IntegerType, [IntegerType]); ("m2", FloatType, [FloatType])]); 
			("B", "C", [("b", FloatType)], []); 
			("C", "Object", [], [])
		]))
let classDefinitionWithCyclicInheritence test_ctxt = assert_raises (Failure "Class B is part of an inheritance cycle")
	(fun () -> checkClass ("A", "B", [], []) ([], [("A", "B", [], []); ("B", "C", [], []); ("C", "B", [], [])]))
let classDefinitionWithDuplicateFields test_ctxt = assert_raises (Failure "Duplicate field name a")
	(fun () -> checkClass ("A", "B", [], []) ([], [("A", "B", [("a", IntegerType)], []); ("B", "C", [("a", FloatType)], []); ("C", "Object", [], [])]))
let multiMethodMultiFieldClassWithDeepInheritanceAndThisUsageAndWrongAssignment test_ctxt = assert_raises (Failure "Incompatible types")
	(fun () -> checkClass 
		("A", "B", [(IntegerType, "a")], [
			(IntegerType, "m1", [(IntegerType, "p")], (ImmediateBlock(ObjectField("this", "b"))));
			(FloatType, "m2", [(FloatType, "p")], (ImmediateBlock(ObjectFieldAssignment("this", "b", Variable("p")))))
		]) 
		([], [
			("A", "B", [("a", IntegerType)], [("m1", IntegerType, [IntegerType]); ("m2", FloatType, [FloatType])]); 
			("B", "C", [("b", FloatType)], []); 
			("C", "Object", [], [])
		]))

(* Class descriptor extraction *)
let singleClassWithNoFieldsMethodsOrInheritanceDescriptorExtraction test_ctxt = assert_equal
	[("A", "Object", [], [])]
	(extractClassDescriptorsFromProgram [("A", "Object", [], [])])
let singleClassWithMultipleFieldsNoMethodsOrInheritanceDescriptorExtraction test_ctxt = assert_equal
	[("A", "Object", [("a", IntegerType); ("b", BooleanType)], [])]
	(extractClassDescriptorsFromProgram [("A", "Object", [(IntegerType, "a"); (BooleanType, "b")], [])])
let singleClassWithMultipleFieldsAndMethodsNoInheritanceDescriptorExtraction test_ctxt = assert_equal
	[("A", "Object", [("a", IntegerType); ("b", BooleanType)], 
		[("m", IntegerType, [IntegerType; IntegerType]); ("n", FloatType, [FloatType; IntegerType])])]
	(extractClassDescriptorsFromProgram [("A", "Object", [(IntegerType, "a"); (BooleanType, "b")], [
		(IntegerType, "m", [(IntegerType, "a"); (IntegerType, "b")], (ImmediateBlock(Variable("a"))));
		(FloatType, "n", [(FloatType, "a"); (IntegerType, "b")], (ImmediateBlock(Variable("a"))))
	])])
let singleClassWithMultipleFieldsAndMethodsAndDeepInheritanceDescriptorExtraction test_ctxt = assert_equal
	[
			("A", "B", [("a", IntegerType)], [("m", IntegerType, [IntegerType; IntegerType])]);
			("B", "C", [], []);
			("C", "Object", [("b", BooleanType)], [("n", FloatType, [FloatType; IntegerType])])
	]
	(extractClassDescriptorsFromProgram [
		("A", "B", [(IntegerType, "a")], [(IntegerType, "m", [(IntegerType, "a"); (IntegerType, "b")], (ImmediateBlock(Variable("a"))))]);
		("B", "C", [], []);
		("C", "Object", [(BooleanType, "b")], [(FloatType, "n", [(FloatType, "a"); (IntegerType, "b")], (ImmediateBlock(Variable("a"))))])
	])
	
(* Check whole program *)
let programWithSingleClassAndEmptyMainMethod test_ctxt = assert_equal ()
	(checkProgram [("A", "Object", [], [(VoidType, "main", [], ImmediateBlock(Sequence([])))])])
let programWithSingleClassAndFieldsWithMainMethod test_ctxt = assert_equal ()
	(checkProgram [("A", "Object", [(IntegerType, "a"); (BooleanType, "b")], [(VoidType, "main", [], ImmediateBlock(Sequence([])))])])
let programWithSeveralClassesAndFieldsWithMainMethod test_ctxt = assert_equal ()
	(checkProgram [
		("C", "B", [(IntegerType, "a"); (BooleanType, "b")], [(VoidType, "m", [], ImmediateBlock(Sequence([Value(NullValue)])))]);
		("B", "Object", [], [(IntegerType, "m", [(ClassName("A"), "a")], ImmediateBlock(Sequence([Value(IntegerValue(1))])))]);
		("A", "Object", [], [(VoidType, "main", [], ImmediateBlock(Sequence([])))])
	])
let programWithoutMainMethodByName test_ctxt = assert_raises (Failure "Unable to find main method")
	(fun () -> checkProgram [("A", "Object", [], [(VoidType, "asd", [], ImmediateBlock(Sequence([])))])])
let programWithoutMainMethodByParams test_ctxt = assert_raises (Failure "Unable to find main method")
	(fun () -> checkProgram [("A", "Object", [], [(VoidType, "main", [(IntegerType, "a")], ImmediateBlock(Sequence([])))])])
let programWithoutMainInLastClass test_ctxt = assert_raises (Failure "Unable to find main method")
	(fun () -> checkProgram [
		("A", "Object", [], [(VoidType, "main", [], ImmediateBlock(Sequence([])))]);
		("B", "Object", [], [(VoidType, "asd", [], ImmediateBlock(Sequence([])))])
	])
let emptyProgramCheck test_ctxt = assert_raises (Failure "Empty program") (fun () -> checkProgram [])
let programWithoutDuplicateClasses test_ctxt = assert_raises (Failure "Duplicate class name A")
	(fun () -> checkProgram [
		("A", "Object", [], [(VoidType, "main", [], ImmediateBlock(Sequence([])))]);
		("A", "Object", [], [(VoidType, "main", [], ImmediateBlock(Sequence([])))])
	])

(* Seminar example (Core Java code) *)
let seminarThreeExampleCorrected test_ctxt = assert_equal () (checkProgram [
	("A", "Object", [(IntegerType, "f1")], [
		(IntegerType, "m1", [(IntegerType, "a"); (IntegerType, "b")], 
  		(LocalVariableBlock(IntegerType, "c", Sequence([
  			VariableAssignment("c", IntegerOperation(Variable("a"), IntegerAddition, Variable("b")));
  			ObjectFieldAssignment("this", "f1", IntegerOperation(ObjectField("this", "f1"), IntegerAddition, Variable("c")));
  			Variable("c")]))
			)
		)
	]);
  ("B", "A", [(ClassName("A"), "f2")], [
		(ClassName("A"), "m2", [(ClassName("A"), "x"); (ClassName("A"), "y")], (
			LocalVariableBlock(ClassName("A"), "z", (
				Sequence([
  				ExpressionBlock(LocalVariableBlock(IntegerType, "n", (
  					Sequence([
  						VariableAssignment("n", IntegerOperation(
  							MethodCall("x", "m1", [Value(IntegerValue(1)); Value(IntegerValue(2))]), 
  							IntegerAddition, 
  							MethodCall("x", "m1", [Value(IntegerValue(2)); Value(IntegerValue(1))])
  						));
      				ExpressionBlock(LocalVariableBlock(BooleanType, "m", (
  							Sequence([
    							VariableAssignment("m", ComparisonOperation(
    								IntegerOperation(ObjectField("x", "f1"), IntegerSubtraction, ObjectField("y", "f1")),
    								GreaterThan,
    								Variable("n")
    							));
  								IfThenElse("m",
										(* in the seminar, for the IF branch, 'm' is passed as parameter; m is bool and f1 is int...  *)
  									ImmediateBlock(VariableAssignment("z", ObjectInstantiation("A", [Value(IntegerValue(1))]))),
  									ImmediateBlock(VariableAssignment("z", ObjectInstantiation("A", [Variable("n")])))
  								)
  							])
  						)))
  					])
  				)));
					ObjectFieldAssignment("this", "f2", Variable("z"));
					Variable("z")
				])
			))
		))
	]);
	("Main", "Object", [], [ (VoidType, "main", [], (
		LocalVariableBlock(ClassName("B"), "o1", (
			Sequence([
				VariableAssignment("o1", ObjectInstantiation("B", [Value(IntegerValue(0)); Value(NullValue)]));
					ExpressionBlock(LocalVariableBlock(ClassName("A"), "o2", (
      			Sequence([
      				VariableAssignment("o2", ObjectInstantiation("A", [Value(IntegerValue(2))]));
      					ExpressionBlock(LocalVariableBlock(ClassName("A"), "o3", (
            			Sequence([
            				VariableAssignment("o3", ObjectInstantiation("A", [Value(IntegerValue(3))]));
            				VariableAssignment("o2", MethodCall("o1", "m2", [Variable("o2"); Variable("o3")]))
            			])
            		)))
      			])
      		)))
			])
		))
	))])
])

let suite = "suite">:::
 [
	"booleanIsAssignableToBoolean">::booleanIsAssignableToBoolean;
	"integerIsAssignableToInteger">::integerIsAssignableToInteger;
	"floatIsAssignableToFloat">::floatIsAssignableToFloat;
	"integerIsAssignableToFloat">::integerIsAssignableToFloat;
	"nullIsAssignableToNull">::nullIsAssignableToNull;
	"classAIsAssignableToClassA">::classAIsAssignableToClassA;
	"classIsAssignableToSuperClass">::classIsAssignableToSuperClass;
	"classIsAssignableToObject">::classIsAssignableToObject;
	"booleanIsNotAssignableToInteger">::booleanIsNotAssignableToInteger;
	"integerIsNotAssignableToBoolean">::integerIsNotAssignableToBoolean;
	"floatIsNotAssignableToInteger">::floatIsNotAssignableToInteger;
	"unrelatedClassesAreNotAssignable">::unrelatedClassesAreNotAssignable;
	"nullIsNotAssignableToInteger">::nullIsNotAssignableToInteger;
	
	"simpleValueReturnsRightType">:: simpleValueReturnsRightType;
	"nullValueReturnsBottomType">:: nullValueReturnsBottomType;
	
	"simpleVariableWithSingletonEnvironment">:: simpleVariableWithSingletonEnvironment;
	"simpleVariableWithLongEnvironment">:: simpleVariableWithLongEnvironment;
	"simpleVariableWithUnknownVar">:: simpleVariableWithUnknownVar;
	
	"simpleVariableAssignment">:: simpleVariableWithSingletonEnvironment;
	"simpleVariableAssignmentWithIncompatibleTypes">:: simpleVariableWithLongEnvironment;
	"simpleVariableAssignmentWithUnknownVar">:: simpleVariableWithUnknownVar;
	
	"simpleIntegerOperationWithBothOperandsInteger">:: simpleIntegerOperationWithBothOperandsInteger;
	"complexIntegerOperationWithBothOperandsInteger">:: complexIntegerOperationWithBothOperandsInteger;
	"simpleIntegerOperationWithFloatOperand">:: simpleIntegerOperationWithFloatOperand;
	"simpleIntegerOperationWithClassOperand">:: simpleIntegerOperationWithClassOperand;
	
	"simpleFloatOperationWithBothOperandsFloat">:: simpleFloatOperationWithBothOperandsFloat;
	"complexFloatOperationWithBothOperandsFloat">:: complexFloatOperationWithBothOperandsFloat;
	"simpleFloatOperationWithOneOperandInteger">:: simpleFloatOperationWithOneOperandInteger;
	"simpleFloatOperationWithClassOperand">:: simpleFloatOperationWithClassOperand;
	
	"simpleBooleanOperationWithBothOperandsBoolean">:: simpleBooleanOperationWithBothOperandsBoolean;
	"complexBooleanOperationWithBothOperandsBoolean">:: complexBooleanOperationWithBothOperandsBoolean;
	"simpleBooleanOperationWithFloatOperand">:: simpleBooleanOperationWithFloatOperand;
	
	"simpleUnaryBooleanOperationWithBooleanOperand">:: simpleUnaryBooleanOperationWithBooleanOperand;
	"simpleUnaryBooleanOperationWithClassOperand">:: simpleUnaryBooleanOperationWithClassOperand;
	
	"simpleImmediateBlockReturnsInnerType">:: simpleImmediateBlockReturnsInnerType;
	"simpleImmediateBlockPropagatesTypeEnv">:: simpleImmediateBlockPropagatesTypeEnv;
	"simpleImmediateBlockPropagatesTypeError">:: simpleImmediateBlockPropagatesTypeError;
	"simpleLocalBlockWithDirectVariableRead">:: simpleLocalBlockWithDirectVariableRead;
	
	"instanceOfCheckWithClassVariable">:: instanceOfCheckWithClassVariable;
	"instanceOfCheckWithNullVariable">:: instanceOfCheckWithNullVariable;
	"instanceOfCheckWithClassVariableAndObjectCheck">:: instanceOfCheckWithClassVariableAndObjectCheck;
	"instanceOfCheckWithUnknownClass">:: instanceOfCheckWithUnknownClass;
	
	"singleElementSequencePropagatesResult">:: singleElementSequencePropagatesResult;
	"multipleElementSequencePropagatesLastResult">:: multipleElementSequencePropagatesLastResult;
	"multipleElementSequencePropagatesErrorWhenInner">:: multipleElementSequencePropagatesErrorWhenInner;
	"multipleElementSequencePropagatesErrorWhenLast">:: multipleElementSequencePropagatesErrorWhenLast;
	
	"whileLoopWithSimpleInnerBlock">:: whileLoopWithSimpleInnerBlock;
	"whileLoopWithWrongVariableType">:: whileLoopWithWrongVariableType;
	"whileLoopWithUnknownVariable">:: whileLoopWithUnknownVariable;
	
	"ifThenElseWithSimpleInnerBlocks">:: ifThenElseWithSimpleInnerBlocks;
	"ifThenElseWithWrongVariableType">:: ifThenElseWithWrongVariableType;
	"ifThenElseWithUnknownVariable">:: ifThenElseWithUnknownVariable;
	
	"comparisonOperationWithBothOperandsFloat">:: comparisonOperationWithBothOperandsFloat;
	"comparisonOperationWithFloatAndIntegerOperands">:: comparisonOperationWithFloatAndIntegerOperands;
	"equalityComparisonOperationWithBothOperandsBoolean">:: equalityComparisonOperationWithBothOperandsBoolean;
	"lowerThanComparisonOperationWithBothOperandsBooleanRaisesError">:: lowerThanComparisonOperationWithBothOperandsBooleanRaisesError;
	"equalityComparisonOperationWithBothOperandsClass">:: equalityComparisonOperationWithBothOperandsClass;
	"equalityComparisonOperationWithClassAndNullOperand">:: equalityComparisonOperationWithClassAndNullOperand;
	"lowerThanComparisonOperationWithBothOperandsClassRaisesErorr">:: lowerThanComparisonOperationWithBothOperandsClassRaisesErorr;
	
	"simpleFieldAssignment">:: simpleFieldAssignment;
	"fieldAssignmentWithInheritance">:: fieldAssignmentWithInheritance;
	"fieldAssignmentWithDeepInheritance">:: fieldAssignmentWithDeepInheritance;
	"fieldAssignmentWithInheritanceAndFieldOverloading">:: fieldAssignmentWithInheritanceAndFieldOverloading;
	"simpleFieldAssignmentWithUnknownField">:: simpleFieldAssignmentWithUnknownField;
	"simpleFieldAssignmentWithUnknownClass">:: simpleFieldAssignmentWithUnknownClass;
	"simpleFieldAssignmentWithWrongType">:: simpleFieldAssignmentWithWrongType;
	
	"simpleFieldAssignment">:: simpleFieldAssignment;
	"fieldAssignmentWithInheritance">:: fieldAssignmentWithInheritance;
	"simpleFieldAssignmentWithUnknownField">:: simpleFieldAssignmentWithUnknownField;
	"simpleFieldAssignmentWithUnknownClass">:: simpleFieldAssignmentWithUnknownClass;
	
	"simpleClassCast">:: simpleClassCast;
	"classCastWithUnknownVar">:: classCastWithUnknownVar;
	"classCastWithUnknownVarClass">:: classCastWithUnknownVarClass;
	"classCastWithUnknownTargetClass">:: classCastWithUnknownTargetClass;
	
	"noArgConstructorWithNoInheritance">:: noArgConstructorWithNoInheritance;
	"noArgConstructorWithDeepInheritance">:: noArgConstructorWithDeepInheritance;
	"multiArgConstructorWithSameTypesAndNoInheritance">:: multiArgConstructorWithSameTypesAndNoInheritance;
	"multiArgConstructorWithCompatibleTypesAndNoInheritance">:: multiArgConstructorWithCompatibleTypesAndNoInheritance;
	"multiArgConstructorWithSameTypesAndDeepInheritance">:: multiArgConstructorWithSameTypesAndDeepInheritance;
	"tooFewArgConstructor">:: tooFewArgConstructor;
	"tooManyArgConstructor">:: tooManyArgConstructor;
	"unknownClassConstructor">:: unknownClassConstructor;
	"wrongArgumentTypeConstructorWithNoInheritance">:: wrongArgumentTypeConstructorWithNoInheritance;
	"wrongArgumentTypeConstructorWithDeepInheritance">:: wrongArgumentTypeConstructorWithDeepInheritance;
	
	"noArgMethodWithNoInheritance">:: noArgMethodWithNoInheritance;
	"noArgMethodWithDeepInheritance">:: noArgMethodWithDeepInheritance;
	"noArgsFullMethodWithIgnoreReturn">:: noArgsFullMethodWithIgnoreReturn;
	"multiArgMethodWithNoInheritance">:: multiArgMethodWithNoInheritance;
	"multiArgMethodWithDeepInheritance">:: multiArgMethodWithDeepInheritance;
	"multiArgMethodWithSimpleOverloading">:: multiArgMethodWithSimpleOverloading;
	"multiArgMethodWithCompatibleTypes">:: multiArgMethodWithCompatibleTypes;
	"methodCallWithUnknownMethodName">:: methodCallWithUnknownMethodName;
	"methodCallWithMismatchingSignatureByNumber">:: methodCallWithMismatchingSignatureByNumber;
	"methodCallWithMismatchingSignatureByType">:: methodCallWithMismatchingSignatureByType;
	
	"noArgsFullMethodWithIntegerReturnType">:: noArgsFullMethodWithIntegerReturnType;
	"noArgsFullMethodWithVoidReturnType">:: noArgsFullMethodWithVoidReturnType;
	"multiArgsFullMethodWithIntegerReturnTypeAndParamUsage">:: multiArgsFullMethodWithIntegerReturnTypeAndParamUsage;
	"multiArgsFullMethodWithWrongReturnType">:: multiArgsFullMethodWithWrongReturnType;
	"multiArgsFullMethodWithDuplicateParamName">:: multiArgsFullMethodWithDuplicateParamName;
	
	"noMethodNoFieldClassWithoutInheritance">:: noMethodNoFieldClassWithoutInheritance;
	"noMethodNoFieldClassWitDeepInheritance">:: noMethodNoFieldClassWitDeepInheritance;
	"noMethodMultiFieldClassWitDeepInheritance">:: noMethodMultiFieldClassWitDeepInheritance;
	"multiMethodMultiFieldClassWitDeepInheritance">:: multiMethodMultiFieldClassWitDeepInheritance;
	"multiMethodMultiFieldClassWithDeepInheritanceAndThisUsage">:: multiMethodMultiFieldClassWithDeepInheritanceAndThisUsage;
	"classDefinitionWithCyclicInheritence">:: classDefinitionWithCyclicInheritence;
	"classDefinitionWithDuplicateFields">:: classDefinitionWithDuplicateFields;
	"multiMethodMultiFieldClassWithDeepInheritanceAndThisUsageAndWrongAssignment">:: multiMethodMultiFieldClassWithDeepInheritanceAndThisUsageAndWrongAssignment;
	
	"singleClassWithNoFieldsMethodsOrInheritanceDescriptorExtraction">:: singleClassWithNoFieldsMethodsOrInheritanceDescriptorExtraction;
	"singleClassWithMultipleFieldsNoMethodsOrInheritanceDescriptorExtraction">:: singleClassWithMultipleFieldsNoMethodsOrInheritanceDescriptorExtraction;
	"singleClassWithMultipleFieldsAndMethodsNoInheritanceDescriptorExtraction">:: singleClassWithMultipleFieldsAndMethodsNoInheritanceDescriptorExtraction;
	"singleClassWithMultipleFieldsAndMethodsAndDeepInheritanceDescriptorExtraction">:: singleClassWithMultipleFieldsAndMethodsAndDeepInheritanceDescriptorExtraction;
	
	"programWithSingleClassAndEmptyMainMethod">:: programWithSingleClassAndEmptyMainMethod;
	"programWithSingleClassAndFieldsWithMainMethod">:: programWithSingleClassAndFieldsWithMainMethod;
	"programWithSeveralClassesAndFieldsWithMainMethod">:: programWithSeveralClassesAndFieldsWithMainMethod;
	"programWithoutMainMethodByName">:: programWithoutMainMethodByName;
	"programWithoutMainMethodByParams">:: programWithoutMainMethodByParams;
	"programWithoutMainInLastClass">:: programWithoutMainInLastClass;
	"emptyProgramCheck">:: emptyProgramCheck;
	"programWithoutDuplicateClasses">:: programWithoutDuplicateClasses;
	
	"seminarThreeExampleCorrected">:: seminarThreeExampleCorrected
 ]
;;

let () =
  run_test_tt_main suite
;;