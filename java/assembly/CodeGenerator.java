package assembly;

import java.util.List;
import java.util.ArrayList;

import compiler.Scope.SymbolTableEntry;
import ast.visitor.AbstractASTVisitor;
import compiler.Compiler;

import ast.*;
import assembly.instructions.*;
import compiler.Scope;

public class CodeGenerator extends AbstractASTVisitor<CodeObject> {

	int intRegCount;
	int floatRegCount;
	static final public char intTempPrefix = 't';
	static final public char floatTempPrefix = 'f';
	
	int loopLabel;
	int elseLabel;
	int outLabel;

	String currFunc;
	
	public CodeGenerator() {
		loopLabel = 0;
		elseLabel = 0;
		outLabel = 0;
		intRegCount = 0;		
		floatRegCount = 0;
	}

	public int getIntRegCount() {
		return intRegCount;
	}

	public int getFloatRegCount() {
		return floatRegCount;
	}
	
	/**
	 * Generate code for Variables
	 * 
	 * Create a code object that just holds a variable
	 * 
	 * Important: add a pointer from the code object to the symbol table entry
	 *            so we know how to generate code for it later (we'll need to find
	 *            the address)
	 * 
	 * Mark the code object as holding a variable, and also as an lval
	 */
	@Override
	protected CodeObject postprocess(VarNode node) {
		
		Scope.SymbolTableEntry sym = node.getSymbol();
		
		CodeObject co = new CodeObject(sym);
		co.lval = true;
		co.type = node.getType();

		return co;
	}

	/** Generate code for IntLiterals
	 * 
	 * Use load immediate instruction to do this.
	 */
	@Override
	protected CodeObject postprocess(IntLitNode node) {
		CodeObject co = new CodeObject();
		
		//Load an immediate into a register
		//The li and la instructions are the same, but it's helpful to distinguish
		//for readability purposes.
		//li tmp' value
		Instruction i = new Li(generateTemp(Scope.InnerType.INT), node.getVal());

		co.code.add(i); //add this instruction to the code object
		co.lval = false; //co holds an rval -- data
		co.temp = i.getDest(); //temp is in destination of li
		co.type = node.getType();

		return co;
	}

	/** Generate code for FloatLiteras
	 * 
	 * Use load immediate instruction to do this.
	 */
	@Override
	protected CodeObject postprocess(FloatLitNode node) {
		CodeObject co = new CodeObject();
		
		//Load an immediate into a regisster
		//The li and la instructions are the same, but it's helpful to distinguish
		//for readability purposes.
		//li tmp' value
		Instruction i = new FImm(generateTemp(Scope.InnerType.FLOAT), node.getVal());

		co.code.add(i); //add this instruction to the code object
		co.lval = false; //co holds an rval -- data
		co.temp = i.getDest(); //temp is in destination of li
		co.type = node.getType();

		return co;
	}

	/**
	 * Generate code for binary operations.
	 * 
	 * Step 0: create new code object
	 * Step 1: add code from left child
	 * Step 1a: if left child is an lval, add a load to get the data
	 * Step 2: add code from right child
	 * Step 2a: if right child is an lval, add a load to get the data
	 * Step 3: generate binary operation using temps from left and right
	 * 
	 * Don't forget to update the temp and lval fields of the code object!
	 * 	   Hint: where is the result stored? Is this data or an address?
	 * 
	 */
	// @Override
	// protected CodeObject postprocess(BinaryOpNode node, CodeObject left, CodeObject right) {

	// 	CodeObject co = new CodeObject();
		
	// 	//Type Checking
	// 	// if (!left.getType().equals(right.getType())){
	// 	// 	System.err.println("Binary TYPE ERROR");
	// 	// 	System.exit(7);
	// 	// }
		
	// 	/* FILL IN FROM STEP 2 */
	// 	if(left.lval){ /*Step 1a: if left child is an lval, add a load to get the data*/
	// 		left = rvalify(left);
	// 		co.code.addAll(left.getCode());
	// 	}
	// 	else{
	// 		co.code.addAll(left.getCode()); /* Step 1: add code from left child*/
	// 	}

	// 	if(right.lval){ /* Step 2a: if right child is an lval, add a load to get the data*/
	// 		right = rvalify(right);
	// 		co.code.addAll(right.getCode());
	// 	}
	// 	else{
	// 		co.code.addAll(right.getCode()); /* Step 2: add code from right child*/
	// 	}
	// 	//Step 3: generate binary operation using temps from left and right
	// 	if(left.getType().type == Scope.InnerType.FLOAT){
	// 		switch(node.getOp()){
	// 			case ADD: co.temp = generateTemp(Scope.InnerType.FLOAT);
	// 					co.code.add(new FAdd(left.temp, right.temp, co.temp));
	// 					break;

	// 			case SUB: co.temp = generateTemp(Scope.InnerType.FLOAT);
	// 					co.code.add(new FSub(left.temp, right.temp, co.temp));
	// 					break;

	// 			case MUL: co.temp = generateTemp(Scope.InnerType.FLOAT);
	// 					co.code.add(new FMul(left.temp, right.temp, co.temp));
	// 					break;

	// 			case DIV: co.temp = generateTemp(Scope.InnerType.FLOAT);
	// 					co.code.add(new FDiv(left.temp, right.temp, co.temp));
	// 					break;
	// 		}
	// 	}
	// 	else{
	// 		switch(node.getOp()){
	// 			case ADD: co.temp = generateTemp(Scope.InnerType.INT);
	// 					co.code.add(new Add(left.temp, right.temp, co.temp));
	// 					break;

	// 			case SUB: co.temp = generateTemp(Scope.InnerType.INT);
	// 					co.code.add(new Sub(left.temp, right.temp, co.temp));
	// 					break;

	// 			case MUL: co.temp = generateTemp(Scope.InnerType.INT);
	// 					co.code.add(new Mul(left.temp, right.temp, co.temp));
	// 					break;

	// 			case DIV: co.temp = generateTemp(Scope.InnerType.INT);
	// 					co.code.add(new Div(left.temp, right.temp, co.temp));
	// 					break;
	// 		}
	// 	}

	// 	co.type = left.getType();
	// 	co.lval = false;

	// 	return co;
	// }
	@Override
	protected CodeObject postprocess(BinaryOpNode node, CodeObject left, CodeObject right) {

    CodeObject co = new CodeObject();

    // Step 1: Check for constant folding opportunities
    if (left.isConstant() && right.isConstant()) {
        // Evaluate the constant expression
        int result = 0;
        float floatResult = 0.0f;

        // Handle different types (INT or FLOAT)
        if (left.getType().type == Scope.InnerType.FLOAT) {
            float leftValue = Float.parseFloat(left.getConstantValue());
            float rightValue = Float.parseFloat(right.getConstantValue());

            switch (node.getOp()) {
                case ADD:
                    floatResult = leftValue + rightValue;
                    break;
                case SUB:
                    floatResult = leftValue - rightValue;
                    break;
                case MUL:
                    floatResult = leftValue * rightValue;
                    break;
                case DIV:
                    floatResult = leftValue / rightValue;
                    break;
                default:
                    throw new UnsupportedOperationException("Unsupported operator for floats: " + node.getOp());
            }

            co.temp = generateTemp(Scope.InnerType.FLOAT);
            co.code.add(new Li(co.temp, String.valueOf(floatResult)));
            co.type = left.getType();
            co.lval = false;
            return co;

        } else {
            int leftValue = Integer.parseInt(left.getConstantValue());
            int rightValue = Integer.parseInt(right.getConstantValue());

            switch (node.getOp()) {
                case ADD:
                    result = leftValue + rightValue;
                    break;
                case SUB:
                    result = leftValue - rightValue;
                    break;
                case MUL:
                    result = leftValue * rightValue;
                    break;
                case DIV:
                    result = leftValue / rightValue;
                    break;
                default:
                    throw new UnsupportedOperationException("Unsupported operator for ints: " + node.getOp());
            }

            co.temp = generateTemp(Scope.InnerType.INT);
            co.code.add(new Li(co.temp, String.valueOf(result)));
            co.type = left.getType();
            co.lval = false;
            return co;
        }
    }

    // Step 2: Normal Binary Operation Code Generation
    if (left.lval) { /*Step 1a: if left child is an lval, add a load to get the data*/
        left = rvalify(left);
        co.code.addAll(left.getCode());
    } else {
        co.code.addAll(left.getCode()); /* Step 1: add code from left child*/
    }

    if (right.lval) { /* Step 2a: if right child is an lval, add a load to get the data*/
        right = rvalify(right);
        co.code.addAll(right.getCode());
    } else {
        co.code.addAll(right.getCode()); /* Step 2: add code from right child*/
    }

    // Step 3: Generate binary operation using temps from left and right
    if (left.getType().type == Scope.InnerType.FLOAT) {
        switch (node.getOp()) {
            case ADD:
                co.temp = generateTemp(Scope.InnerType.FLOAT);
                co.code.add(new FAdd(left.temp, right.temp, co.temp));
                break;

            case SUB:
                co.temp = generateTemp(Scope.InnerType.FLOAT);
                co.code.add(new FSub(left.temp, right.temp, co.temp));
                break;

            case MUL:
                co.temp = generateTemp(Scope.InnerType.FLOAT);
                co.code.add(new FMul(left.temp, right.temp, co.temp));
                break;

            case DIV:
                co.temp = generateTemp(Scope.InnerType.FLOAT);
                co.code.add(new FDiv(left.temp, right.temp, co.temp));
                break;
        }
    } else {
        switch (node.getOp()) {
            case ADD:
                co.temp = generateTemp(Scope.InnerType.INT);
                co.code.add(new Add(left.temp, right.temp, co.temp));
                break;

            case SUB:
                co.temp = generateTemp(Scope.InnerType.INT);
                co.code.add(new Sub(left.temp, right.temp, co.temp));
                break;

            case MUL:
                co.temp = generateTemp(Scope.InnerType.INT);
                co.code.add(new Mul(left.temp, right.temp, co.temp));
                break;

            case DIV:
                co.temp = generateTemp(Scope.InnerType.INT);
                co.code.add(new Div(left.temp, right.temp, co.temp));
                break;
        }
    }

    co.type = left.getType();
    co.lval = false;

    return co;
}


	/**
	 * Generate code for unary operations.
	 * 
	 * Step 0: create new code object
	 * Step 1: add code from child expression
	 * Step 1a: if child is an lval, add a load to get the data
	 * Step 2: generate instruction to perform unary operation
	 * 
	 * Don't forget to update the temp and lval fields of the code object!
	 * 	   Hint: where is the result stored? Is this data or an address?
	 * 
	 */
	@Override
	protected CodeObject postprocess(UnaryOpNode node, CodeObject expr) {
		
		CodeObject co = new CodeObject();

		/* FILL IN FROM STEP 2 */
		//Step 1: add code from child expression
		if(expr.lval){ //Step 1a: if child is an lval, add a load to get the data
			expr = rvalify(expr);
			co.code.addAll(expr.getCode());
		}
		else{
			co.code.addAll(expr.getCode());
		}

		//Step 2: generate instruction to perform unary operation (don't forget to generate right type of op)
		if(expr.getType().type == Scope.InnerType.INT){
			co.temp = generateTemp(Scope.InnerType.INT);
			co.code.add(new Neg(expr.temp, co.temp));
		}
		else if(expr.getType().type == Scope.InnerType.FLOAT){
			co.temp = generateTemp(Scope.InnerType.FLOAT);
			co.code.add(new FNeg(expr.temp, co.temp));
		}

		co.lval = false;
		co.type = expr.getType();

		return co;
	}

	/**
	 * Generate code for assignment statements
	 * 
	 * Step 0: create new code object
	 * Step 1: if LHS is a variable, generate a load instruction to get the address into a register
	 * Step 1a: add code from LHS of assignment (make sure it results in an lval!)
	 * Step 2: add code from RHS of assignment
	 * Step 2a: if right child is an lval, add a load to get the data
	 * Step 3: generate store
	 * 
	 * Hint: it is going to be easiest to just generate a store with a 0 immediate
	 * offset, and the complete store address in a register:
	 * 
	 * sw rhs 0(lhs)
	 */
	@Override
	protected CodeObject postprocess(AssignNode node, CodeObject left,
			CodeObject right) {
		
		CodeObject co = new CodeObject();

		//Type Checking
		// if (!left.getType().equals(right.getType())){
		// 	System.err.println("Assignment TYPE ERROR");
		// 	System.exit(7);
		// }

		/* FILL IN FROM STEP 2 */
		if (left.isVar()) {
			InstructionList il = generateAddrFromVariable(left);
			left.temp = il.getLast().getDest();
			co.code.addAll(il);
		}

		/* FILL IN FOR STEP 2 */
		//Step 1b: add code from LHS of assignment (make sure it results in an lval!)
		co.code.addAll(left.code);

		//* Step 2: add code from RHS of assignment
	 	//* Step 2a: if right child is an lval, add a load to get the data
		if(right.lval){
			right = rvalify(right);
		}
		co.code.addAll(right.code);

		//Step 3: generate store (don't forget to generate the right type of store)
		if(left.getType().type == Scope.InnerType.FLOAT){
			co.code.add(new Fsw(right.temp, left.temp, "0"));
		}
		else{
			co.code.add(new Sw(right.temp, left.temp, "0"));
		}

		co.lval = true;
		co.temp = left.temp;
		
		if(left.getType().type == Scope.InnerType.PTR){
			co.type = left.getType().getWrappedType();
		}
		else{
			co.type = left.getType();
		}
		
		return co;
	}

	/**
	 * Add together all the lists of instructions generated by the children
	 */
	@Override
	protected CodeObject postprocess(StatementListNode node,
			List<CodeObject> statements) {
		CodeObject co = new CodeObject();
		//add the code from each individual statement
		for (CodeObject subcode : statements) {
			co.code.addAll(subcode.code);
		}
		co.type = null; //set to null to trigger errors
		return co;
	}
	
	/**
	 * Generate code for read
	 * 
	 * Step 0: create new code object
	 * Step 1: add code from VarNode (make sure it's an lval)
	 * Step 2: generate GetI instruction, storing into temp
	 * Step 3: generate store, to store temp in variable
	 */
	@Override
	protected CodeObject postprocess(ReadNode node, CodeObject var) {
		
		//Step 0
		CodeObject co = new CodeObject();

		//Generating code for read(id)
		assert(var.getSTE() != null); //var had better be a variable

		InstructionList il = new InstructionList();
		switch(node.getType().type) {
			case INT: 
				//Code to generate if INT:
				//geti tmp
				//if var is global: la tmp', <var>; sw tmp 0(tmp')
				//if var is local: sw tmp offset(fp)
				Instruction geti = new GetI(generateTemp(Scope.InnerType.INT));
				il.add(geti);
				InstructionList store = new InstructionList();
				if (var.getSTE().isLocal()) {
					store.add(new Sw(geti.getDest(), "fp", String.valueOf(var.getSTE().addressToString())));
				} else {
					store.addAll(generateAddrFromVariable(var));
					store.add(new Sw(geti.getDest(), store.getLast().getDest(), "0"));
				}
				il.addAll(store);
				break;
			case FLOAT:
				//Code to generate if FLOAT:
				//getf tmp
				//if var is global: la tmp', <var>; fsw tmp 0(tmp')
				//if var is local: fsw tmp offset(fp)
				Instruction getf = new GetF(generateTemp(Scope.InnerType.FLOAT));
				il.add(getf);
				InstructionList fstore = new InstructionList();
				if (var.getSTE().isLocal()) {
					fstore.add(new Fsw(getf.getDest(), "fp", String.valueOf(var.getSTE().addressToString())));
				} else {
					fstore.addAll(generateAddrFromVariable(var));
					fstore.add(new Fsw(getf.getDest(), fstore.getLast().getDest(), "0"));
				}
				il.addAll(fstore);
				break;
			default:
				throw new Error("Shouldn't read into other variable");
		}
		
		co.code.addAll(il);

		co.lval = false; //doesn't matter
		co.temp = null; //set to null to trigger errors
		co.type = null; //set to null to trigger errors

		return co;
	}

	/**
	 * Generate code for print
	 * 
	 * Step 0: create new code object
	 * 
	 * If printing a string:
	 * Step 1: add code from expression to be printed (make sure it's an lval)
	 * Step 2: generate a PutS instruction printing the result of the expression
	 * 
	 * If printing an integer:
	 * Step 1: add code from the expression to be printed
	 * Step 1a: if it's an lval, generate a load to get the data
	 * Step 2: Generate PutI that prints the temporary holding the expression
	 */
	@Override
	protected CodeObject postprocess(WriteNode node, CodeObject expr) {
		CodeObject co = new CodeObject();

		//generating code for write(expr)

		//for strings, we expect a variable
		if (node.getWriteExpr().getType().type == Scope.InnerType.STRING) {
			//Step 1:
			assert(expr.getSTE() != null);
			
			System.out.println("; generating code to print " + expr.getSTE());

			//Get the address of the variable
			InstructionList addrCo = generateAddrFromVariable(expr);
			co.code.addAll(addrCo);

			//Step 2:
			Instruction write = new PutS(addrCo.getLast().getDest());
			co.code.add(write);
		} else {
			//Step 1a:
			//if expr is an lval, load from it
			if (expr.lval == true) {
				expr = rvalify(expr);
			}
			
			//Step 1:
			co.code.addAll(expr.code);

			//Step 2:
			//if type of writenode is int, use puti, if float, use putf
			Instruction write = null;
			switch(node.getWriteExpr().getType().type) {
			case STRING: throw new Error("Shouldn't have a STRING here");
			case INT: 
			case PTR: //should work the same way for pointers
				write = new PutI(expr.temp); break;
			case FLOAT: write = new PutF(expr.temp); break;
			default: throw new Error("WriteNode has a weird type");
			}

			co.code.add(write);
		}

		co.lval = false; //doesn't matter
		co.temp = null; //set to null to trigger errors
		co.type = null; //set to null to trigger errors

		return co;
	}

	/**
	 * FILL IN FROM STEP 3
	 * 
	 * Generating an instruction sequence for a conditional expression
	 * 
	 * Implement this however you like. One suggestion:
	 *
	 * Create the code for the left and right side of the conditional, but defer
	 * generating the branch until you process IfStatementNode or WhileNode (since you
	 * do not know the labels yet). Modify CodeObject so you can save the necessary
	 * information to generate the branch instruction in IfStatementNode or WhileNode
	 * 
	 * Alternate idea 1:
	 * 
	 * Don't do anything as part of CodeGenerator. Create a new visitor class
	 * that you invoke *within* your processing of IfStatementNode or WhileNode
	 * 
	 * Alternate idea 2:
	 * 
	 * Create the branch instruction in this function, then tweak it as necessary in
	 * IfStatementNode or WhileNode
	 * 
	 * Hint: you may need to preserve extra information in the returned CodeObject to
	 * make sure you know the type of branch code to generate (int vs float)
	 */
	@Override
	protected CodeObject postprocess(CondNode node, CodeObject left, CodeObject right) {
		CodeObject co = new CodeObject();

		/* FILL IN FROM STEP 3*/
		//Type Checking
		// if (!left.getType().equals(right.getType())){
		// 	System.err.println("Conditional TYPE ERROR");
		// 	System.exit(7);
		// }

		/* FILL IN FROM STEP 3*/
		co.node = node;
		co.Leftnode = left;
		co.Rightnode = right;

		return co;
	}

	/**
	 * FILL IN FROM STEP 3
	 * 
	 * Step 0: Create code object
	 * 
	 * Step 1: generate labels
	 * 
	 * Step 2: add code from conditional expression
	 * 
	 * Step 3: create branch statement (if not created as part of step 2)
	 * 			don't forget to generate correct branch based on type
	 * 
	 * Step 4: generate code
	 * 		<cond code>
	 *		<flipped branch> elseLabel
	 *		<then code>
	 *		j outLabel
	 *		elseLabel:
	 *		<else code>
	 *		outLabel:
	 *
	 * Step 5 insert code into code object in appropriate order.
	 */
	@Override
	protected CodeObject postprocess(IfStatementNode node, CodeObject cond, CodeObject tlist, CodeObject elist) {
		//Step 0:
		CodeObject co = new CodeObject();

		/* FILL IN FROM STEP 3*/
		//Step 1: generate labels
		String outlabel = generateOutLabel();
		String elselabel = generateElseLabel();

		//Step 2: add code from conditional expression
		if(cond.Leftnode.lval){
			CodeObject left = rvalify(cond.Leftnode);
			co.code.addAll(left.getCode());
			cond.Leftnode.temp = left.temp;
		}
		else{
			co.code.addAll(cond.Leftnode.getCode());
		}

		if(cond.Rightnode.lval){
			CodeObject right = rvalify(cond.Rightnode);
			co.code.addAll(right.getCode());
			cond.Rightnode.temp = right.temp;
		}
		else{
			co.code.addAll(cond.Rightnode.getCode());
		}
		//Step 3: create branch statement
		Instruction reverse_instr;
		Instruction branch_instr;
		String zero_temp = generateTemp(Scope.InnerType.INT);
		Instruction zero_instr = new Li(zero_temp, "0");

		if(cond.Leftnode.getType().type == Scope.InnerType.INT){
			switch(cond.node.getReversedOp()){
				case LE: reverse_instr = new Ble(cond.Leftnode.temp, cond.Rightnode.temp, elselabel);
				break;
				case LT: reverse_instr =new Blt(cond.Leftnode.temp, cond.Rightnode.temp, elselabel);
				break;
				case GT: reverse_instr =new Bgt(cond.Leftnode.temp, cond.Rightnode.temp, elselabel);
				break;
				case GE: reverse_instr =new Bge(cond.Leftnode.temp, cond.Rightnode.temp, elselabel);
				break;
				case EQ: reverse_instr =new Beq(cond.Leftnode.temp, cond.Rightnode.temp, elselabel);
				break;
				case NE: reverse_instr =new Bne(cond.Leftnode.temp, cond.Rightnode.temp, elselabel);
				break;
				default: reverse_instr=null;
			}

			co.code.add(reverse_instr);
		}
		else if(cond.Leftnode.getType().type == Scope.InnerType.FLOAT){
			switch(cond.node.getReversedOp()){
				case LE: co.temp = generateTemp(Scope.InnerType.INT);
						reverse_instr =new Fle(cond.Leftnode.temp, cond.Rightnode.temp, co.temp);
						// co.code.add(zero_instr);
						branch_instr = new Bne(co.temp, zero_temp, elselabel);
				break;
				case LT: co.temp = generateTemp(Scope.InnerType.INT);
						reverse_instr =new Flt(cond.Leftnode.temp, cond.Rightnode.temp, co.temp);
						// co.code.add(zero_instr);
						branch_instr = new Bne(co.temp, zero_temp, elselabel);
				break;
				case GT: co.temp = generateTemp(Scope.InnerType.INT);
						reverse_instr =new Fle(cond.Leftnode.temp, cond.Rightnode.temp, co.temp);
						// co.code.add(zero_instr);
						branch_instr = new Beq(co.temp, zero_temp, elselabel);
				break;
				case GE: co.temp = generateTemp(Scope.InnerType.INT);
						reverse_instr =new Flt(cond.Leftnode.temp, cond.Rightnode.temp, co.temp);
						// co.code.add(zero_instr);
						branch_instr = new Beq(co.temp, zero_temp, elselabel);
				break;
				case EQ: co.temp = generateTemp(Scope.InnerType.INT);
						reverse_instr =new Feq(cond.Leftnode.temp, cond.Rightnode.temp, co.temp);
						// co.code.add(zero_instr);
						branch_instr = new Bne(co.temp, zero_temp, elselabel);
				break;
				case NE: co.temp = generateTemp(Scope.InnerType.INT);
						reverse_instr =new Feq(cond.Leftnode.temp, cond.Rightnode.temp, co.temp);
						// co.code.add(zero_instr);
						branch_instr = new Beq(co.temp, zero_temp, elselabel);
				break;
				default: reverse_instr=null;
						branch_instr = null;
			}

			co.code.add(reverse_instr);
			co.code.add(zero_instr);
			co.code.add(branch_instr);
		}

		co.code.addAll(tlist.getCode());
		co.code.add(new J(outlabel));
		co.code.add(new Label(elselabel));
		co.code.addAll(elist.getCode());
		co.code.add(new Label(outlabel));

		return co;
	}

		/**
	 * FILL IN FROM STEP 3
	 * 
	 * Step 0: Create code object
	 * 
	 * Step 1: generate labels
	 * 
	 * Step 2: add code from conditional expression
	 * 
	 * Step 3: create branch statement (if not created as part of step 2)
	 * 			don't forget to generate correct branch based on type
	 * 
	 * Step 4: generate code
	 * 		loopLabel:
	 *		<cond code>
	 *		<flipped branch> outLabel
	 *		<body code>
	 *		j loopLabel
	 *		outLabel:
	 *
	 * Step 5 insert code into code object in appropriate order.
	 */
	@Override
	protected CodeObject postprocess(WhileNode node, CodeObject cond, CodeObject slist) {
		//Step 0:
		CodeObject co = new CodeObject();

		/* FILL IN FROM STEP 3*/
		//Step 1: generate labels
		String outlabel = generateOutLabel();
		String looplabel = generateLoopLabel();

		co.code.add(new Label(looplabel));

		//Step 2: add code from conditional expression
		if(cond.Leftnode.lval){
			CodeObject left = rvalify(cond.Leftnode);
			co.code.addAll(left.getCode());
			cond.Leftnode.temp = left.temp;
		}
		else{
			co.code.addAll(cond.Leftnode.getCode());
		}

		if(cond.Rightnode.lval){
			CodeObject right = rvalify(cond.Rightnode);
			co.code.addAll(right.getCode());
			cond.Rightnode.temp = right.temp;
		}
		else{
			co.code.addAll(cond.Rightnode.getCode());
		}
		//Step 3: create branch statement
		Instruction reverse_instr;
		Instruction branch_instr;
		String zero_temp = generateTemp(Scope.InnerType.INT);
		Instruction zero_instr = new Li(zero_temp, "0");

		
		if(cond.Leftnode.getType().type == Scope.InnerType.FLOAT){
			switch(cond.node.getReversedOp()){
				case LE: co.temp = generateTemp(Scope.InnerType.INT);
						reverse_instr = new Fle(cond.Leftnode.temp, cond.Rightnode.temp, co.temp);
						branch_instr = new Bne(co.temp, zero_temp, outlabel);
				break;
				case LT: co.temp = generateTemp(Scope.InnerType.INT);
						reverse_instr = new Flt(cond.Leftnode.temp, cond.Rightnode.temp, co.temp);
						branch_instr = new Bne(co.temp, zero_temp, outlabel);
				break;
				case GT: co.temp = generateTemp(Scope.InnerType.INT);
						reverse_instr = new Fle(cond.Leftnode.temp, cond.Rightnode.temp, co.temp);
						branch_instr = new Beq(co.temp, zero_temp, outlabel);
				break;
				case GE: co.temp = generateTemp(Scope.InnerType.INT);
						reverse_instr = new Flt(cond.Leftnode.temp, cond.Rightnode.temp, co.temp);
						branch_instr = new Beq(co.temp, zero_temp, outlabel);
				break;
				case EQ: co.temp = generateTemp(Scope.InnerType.INT);
						reverse_instr = new Feq(cond.Leftnode.temp, cond.Rightnode.temp, co.temp);
						branch_instr = new Bne(co.temp, zero_temp, outlabel);
				break;
				case NE: co.temp = generateTemp(Scope.InnerType.INT);
						reverse_instr = new Feq(cond.Leftnode.temp, cond.Rightnode.temp, co.temp);
						branch_instr = new Beq(co.temp, zero_temp, outlabel);
				break;
				default: reverse_instr=null;
						branch_instr = null;
			}

			co.code.add(reverse_instr);
			co.code.add(zero_instr);
			co.code.add(branch_instr);
		}
		else{
			switch(cond.node.getReversedOp()){
				case LE: reverse_instr = new Ble(cond.Leftnode.temp, cond.Rightnode.temp, outlabel);
				break;
				case LT: reverse_instr = new Blt(cond.Leftnode.temp, cond.Rightnode.temp, outlabel);
				break;
				case GT: reverse_instr = new Bgt(cond.Leftnode.temp, cond.Rightnode.temp, outlabel);
				break;
				case GE: reverse_instr = new Bge(cond.Leftnode.temp, cond.Rightnode.temp, outlabel);
				break;
				case EQ: reverse_instr = new Beq(cond.Leftnode.temp, cond.Rightnode.temp, outlabel);
				break;
				case NE: reverse_instr = new Bne(cond.Leftnode.temp, cond.Rightnode.temp, outlabel);
				break;
				default: reverse_instr=null;
			}

			co.code.add(reverse_instr);
		}

		co.code.addAll(slist.getCode());
		co.code.add(new J(looplabel));
		co.code.add(new Label(outlabel));

		return co;
	}
	protected InstructionList pushRegisteri(String s){
		InstructionList instr_list = new InstructionList();
		
		instr_list.add(new Sw(s, "sp", "0"));
		instr_list.add(new Addi("sp", "-4", "sp"));
		return instr_list;
	}

	protected InstructionList pushRegisterf(String s){
		InstructionList instr_list = new InstructionList();

		instr_list.add(new Fsw(s, "sp", "0"));
		instr_list.add(new Addi("sp", "-4", "sp"));

		return instr_list;
	}

	protected InstructionList popRegisteri(String s){
		InstructionList instr_list = new InstructionList();

		instr_list.add(new Addi("sp", "4", "sp"));
		instr_list.add(new Lw(s,"sp", "0"));

		return instr_list;
	}

	protected InstructionList popRegisterf(String s){
		InstructionList instr_list = new InstructionList();

		instr_list.add(new Addi("sp", "4", "sp"));
		instr_list.add(new Flw(s,"sp", "0"));

		return instr_list;
	}
	/**
	 * FILL IN FOR STEP 4
	 * 
	 * Generating code for returns
	 * 
	 * Step 0: Generate new code object
	 * 
	 * Step 1: Add retExpr code to code object (rvalify if necessary)
	 * 
	 * Step 2: Store result of retExpr in appropriate place on stack (fp + 8)
	 * 
	 * Step 3: Jump to out label (use @link{generateFunctionOutLabel()})
	 */
	@Override
	protected CodeObject postprocess(ReturnNode node, CodeObject retExpr) {
		CodeObject co = new CodeObject();

		//Type Checking
		// if(node.getFuncSymbol().getReturnType() != retExpr.getType()){
		// 	System.err.println("Return TYPE ERROR");
		// 	System.exit(7);
		// }

		/* FILL IN */
		//Step 1: Add retExpr code to code object (rvalify if necessary)
		if(retExpr == null) { 
			co.code.add(new J(generateFunctionOutLabel()));
			co.type = new Scope.Type(Scope.InnerType.VOID);
			return co;
		};
		//don't foget to check rvalify
		if(retExpr.lval){
			retExpr = rvalify(retExpr);
			co.code.addAll(retExpr.getCode());
		}
		co.code.addAll(retExpr.code);

		//Step 2: Store result of retExpr in appropriate place on stack (fp + 8)
		Instruction add_stack;
		if(node.getFuncSymbol().getReturnType().type == Scope.InnerType.FLOAT){
			add_stack = new Fsw(retExpr.temp, "fp", "8");
		}
		else{
			add_stack = new Sw(retExpr.temp, "fp", "8");
		}

		co.code.add(add_stack);

		//Step 3: Jump to out label (use @link{generateFunctionOutLabel()})
		Instruction func_out;
		func_out = new J(generateFunctionOutLabel());
		co.code.add(func_out);
		co.type = retExpr.getType();

		return co;
	}

	@Override
	protected void preprocess(FunctionNode node) {
		// Generate function label information, used for other labels inside function
		currFunc = node.getFuncName();

		//reset register counts; each function uses new registers!
		intRegCount = 0;
		floatRegCount = 0;
	}

	/**
	 * FILL IN FOR STEP 4
	 * 
	 * Generate code for functions
	 * 
	 * Step 1: add the label for the beginning of the function
	 * 
	 * Step 2: manage frame  pointer
	 * 			a. Save old frame pointer
	 * 			b. Move frame pointer to point to base of activation record (current sp)
	 * 			c. Update stack pointer
	 * 
	 * Step 3: allocate new stack frame (use scope infromation from FunctionNode)
	 * 
	 * Step 4: save registers on stack (Can inspect intRegCount and floatRegCount to know what to save)
	 * 
	 * Step 5: add the code from the function body
	 * 
	 * Step 6: add post-processing code:
	 * 			a. Label for `return` statements inside function body to jump to
	 * 			b. Restore registers
	 * 			c. Deallocate stack frame (set stack pointer to frame pointer)
	 * 			d. Reset fp to old location
	 * 			e. Return from function
	 */
	@Override
	protected CodeObject postprocess(FunctionNode node, CodeObject body) {
		CodeObject co = new CodeObject();

		// System.out.println("Generate code for functions");

		/* FILL IN */
		//Step 1: add the label for the beginning of the function
		String func_label = generateFunctionLabel();
		Instruction new_label;
		new_label = new Label(func_label);
		co.code.add(new_label);
		//Step 2: manage frame  pointer
	  			//a. Save old frame pointer
	  			//b. Move frame pointer to point to base of activation record (current sp)
	 			//c. Update stack pointer
		Instruction save_old_fp;
		Instruction move_fp_sp;
		Instruction update_sp;
		save_old_fp = new Sw("fp", "sp", "0");
		move_fp_sp = new Mv("sp", "fp");
		update_sp = new Addi("sp", "-4", "sp" );
		co.code.add(save_old_fp);
		co.code.add(move_fp_sp);
		co.code.add(update_sp);
		//Step 3: allocate new stack frame (use scope infromation from FunctionNode)
		String stack_number;
		stack_number = String.valueOf(-4 * node.getScope().getNumLocals());
		Instruction allocate_sf;
		allocate_sf = new Addi("sp", stack_number, "sp");
		co.code.add(allocate_sf);
		//Step 4: save registers on stack (Can inspect intRegCount and floatRegCount to know what to save)
		for(int i = 1; i <= intRegCount; i++){
			co.code.addAll(pushRegisteri("t" + i));
		}
		for(int i = 1; i <= floatRegCount; i++){
			co.code.addAll(pushRegisterf("f" + i));
		}
		//Step 5: add the code from the function body
		if(body.lval){
			body = rvalify(body);
		}
		co.code.addAll(body.code);
		//Step 6: add post-processing code:
	  			//a. Label for `return` statements inside function body to jump to
	  			//b. Restore registers
	  			//c. Deallocate stack frame (set stack pointer to frame pointer)
	  			//d. Reset fp to old location
	  			//e. Return from function
		String post_process = generateFunctionOutLabel();
		Instruction return_label;
		return_label = new Label(post_process);
		co.code.add(return_label);
		//restore
		for(int i = floatRegCount; i >= 1; i--){
			co.code.addAll(popRegisterf("f" + i));
		}
		for(int i = intRegCount; i >= 1; i--){
			co.code.addAll(popRegisteri("t" + i));
		}
		Instruction move_sp_fp;
		Instruction load_fp;
		Instruction reset_fp;
		move_sp_fp = new Mv("fp", "sp");
		load_fp = new Lw("fp", "fp", "0");
		reset_fp = new Ret();
		co.code.add(move_sp_fp);
		co.code.add(load_fp);
		co.code.add(reset_fp);

		co.type = body.getType();
		co.lval = false;

		return co;
	}

	/**
	 * Generate code for the list of functions. This is the "top level" code generation function
	 * 
	 * Step 1: Set fp to point to sp
	 * 
	 * Step 2: Insert a JR to main
	 * 
	 * Step 3: Insert a HALT
	 * 
	 * Step 4: Include all the code of the functions
	 */
	@Override
	protected CodeObject postprocess(FunctionListNode node, List<CodeObject> funcs) {
		CodeObject co = new CodeObject();

		co.code.add(new Mv("sp", "fp"));
		co.code.add(new Jr(generateFunctionLabel("main")));
		co.code.add(new Halt());
		co.code.add(new Blank());

		//add code for each of the functions
		for (CodeObject c : funcs) {
			co.code.addAll(c.code);
			co.code.add(new Blank());
		}

		return co;
	}

	/**
	* 
	* FILL IN FOR STEP 4
	* 
	* Generate code for a call expression
	 * 
	 * Step 1: For each argument:
	 * 
	 * 	Step 1a: insert code of argument (don't forget to rvalify!)
	 * 
	 * 	Step 1b: push result of argument onto stack 
	 * 
	 * Step 2: alloate space for return value
	 * 
	 * Step 3: push current return address onto stack
	 * 
	 * Step 4: jump to function
	 * 
	 * Step 5: pop return address back from stack
	 * 
	 * Step 6: pop return value into fresh temporary (destination of call expression)
	 * 
	 * Step 7: remove arguments from stack (move sp)
	 * 
	 * Add special handling for malloc and free
	 */

	 /**
	  * FOR STEP 6: Make sure to handle VOID functions properly
	  */
	@Override
	protected CodeObject postprocess(CallNode node, List<CodeObject> args) {
		
		//STEP 0
		CodeObject co = new CodeObject();

		System.out.println("Generate code for a call expression");

		/* FILL IN FROM STEP 4 */
		/* FILL IN */
		//Type Checking
		// for(int idx = 0; idx < args.size(); idx++) {
		// 	CodeObject arg = args.get(idx);
		// 	// Insert Argument Code
		// 	if(arg.lval == true) { arg = rvalify(arg); }
		// 	co.code.addAll(arg.code);
		// 	// Loading Result Onto Stack
		// 	if(arg.getType().type == Scope.InnerType.FLOAT) { co.code.add(new Fsw(arg.temp, "sp", "0")); } 
		// 	else { co.code.add(new Sw(arg.temp, "sp", "0")); }
		// 	co.code.add(new Addi("sp", "-4", "sp"));
		// }
		for(int i = 0; i < args.size(); i++) {
			CodeObject arg = args.get(i);
			if(arg.lval) { 
				arg = rvalify(arg); 
			}
			co.code.addAll(arg.getCode());
			
			if(arg.getType().type == Scope.InnerType.FLOAT) { 
				co.code.add(new Fsw(arg.temp, "sp", "0")); 
			} 
			else { 
				co.code.add(new Sw(arg.temp, "sp", "0")); 
			}
			co.code.add(new Addi("sp", "-4", "sp"));
		}
		// Allocate Space For Return Value
		co.code.add(new Addi("sp", "-4", "sp"));
		// Push Current Return Address On Stack
		co.code.add(new Sw("ra", "sp", "0"));
		co.code.add(new Addi("sp", "-4", "sp"));
		// Jump To Function
		co.code.add(new Jr(generateFunctionLabel(node.getFuncName())));
		// Pop Return Address Back From Stack
		co.code.add(new Addi("sp", "4", "sp"));
		// Pop Return Value Into Fresh Temporary
		co.code.add(new Lw("ra", "sp", "0"));
		co.code.add(new Addi("sp", "4", "sp"));
		Instruction loadReturnVal = null;
		if(node.getType().type == Scope.InnerType.FLOAT) { 
			loadReturnVal = new Flw(generateTemp(node.getType().type), "sp", "0"); 
			co.temp = loadReturnVal.getDest();
			co.code.add(loadReturnVal);
		} else if(node.getType().type == Scope.InnerType.INT) { 
			loadReturnVal = new Lw(generateTemp(Scope.InnerType.INT), "sp", "0"); 
			co.temp = loadReturnVal.getDest();
			co.code.add(loadReturnVal);
		} else if(node.getType().type == Scope.InnerType.PTR) {
			loadReturnVal = new Lw(generateTemp(Scope.InnerType.INT), "sp", "0"); 
			co.temp = loadReturnVal.getDest();
			co.code.add(loadReturnVal);
		} else { // Pointer
			co.temp = null;
		}
		// Remove Arguments From Stack
		co.code.add(new Addi("sp", Integer.toString(4 * args.size()), "sp"));
		co.type = node.getType();
		return co;
	}	
	
	/**
	 * Generate code for * (expr)
	 * 
	 * Goal: convert the r-val coming from expr (a computed address) into an l-val (an address that can be loaded/stored)
	 * 
	 * Step 0: Create new code object
	 * 
	 * Step 1: Rvalify expr if needed
	 * 
	 * Step 2: Copy code from expr (including any rvalification) into new code object
	 * 
	 * Step 3: New code object has same temporary as old code, but now is marked as an l-val
	 * 
	 * Step 4: New code object has an "unwrapped" type: if type of expr is * T, type of temporary is T. Can get this from node
	 */
	@Override
	protected CodeObject postprocess(PtrDerefNode node, CodeObject expr) {
		CodeObject co = new CodeObject();

		/* FILL IN FOR STEP 6 */
		if(expr.lval) { 
			expr = rvalify(expr);
		}
		co.code.addAll(expr.code);
		co.temp = expr.temp;
		co.lval = true;
		co.type = expr.getType().getWrappedType();
		return co;
	}

	/**
	 * Generate code for a & (expr)
	 * 
	 * Goal: convert the lval coming from expr (an address) to an r-val (a piece of data that can be used)
	 * 
	 * Step 0: Create new code object
	 * 
	 * Step 1: If lval is a variable, generate code to put address into a register (e.g., generateAddressFromVar)
	 *			Otherwise just copy code from other code object
	 * 
	 * Step 2: New code object has same temporary as existing code, but is an r-val
	 * 
	 * Step 3: New code object has a "wrapped" type. If type of expr is T, type of temporary is *T. Can get this from node
	 */
	@Override
	protected CodeObject postprocess(AddrOfNode node, CodeObject expr) {
		CodeObject co = new CodeObject();

		if(expr.lval && expr.isVar()){
			Instruction load_rg = new Addi("fp", expr.getSTE().addressToString(), generateTemp(Scope.InnerType.INT));
			co.code.add(load_rg);
			expr.temp = load_rg.getDest();
		}

		co.code.addAll(expr.getCode());
		co.temp = expr.temp;
		co.lval = false;
		co.type = Scope.Type.pointerToType(expr.getType());

		return co;
	}

	/**
	 * Generate code for malloc
	 * 
	 * Step 0: Create new code object
	 * 
	 * Step 1: Add code from expression (rvalify if needed)
	 * 
	 * Step 2: Create new MALLOC instruction
	 * 
	 * Step 3: Set code object type to INFER
	 */
	@Override
	protected CodeObject postprocess(MallocNode node, CodeObject expr) {
		CodeObject co = new CodeObject();

		/* FILL IN FOR STEP 6 */
		if(expr.lval == true) { 
			expr = rvalify(expr); 
		}
		co.code.addAll(expr.code);
		String temp = generateTemp(Scope.InnerType.INT); 
		Instruction MallocI = new Malloc(expr.temp, temp);
		co.code.add(MallocI);
		co.temp = temp;
		co.type = new Scope.Type(Scope.InnerType.INFER);
		co.lval = false;
		return co;
	}
	
	/**
	 * Generate code for free
	 * 
	 * Step 0: Create new code object
	 * 
	 * Step 1: Add code from expression (rvalify if needed)
	 * 
	 * Step 2: Create new FREE instruction
	 */
	@Override
	protected CodeObject postprocess(FreeNode node, CodeObject expr) {
		CodeObject co = new CodeObject();

		/* FILL IN FOR STEP 6 */
		if(expr.lval == true) { 
			expr = rvalify(expr); 
		}
		co.code.addAll(expr.code);
		co.code.add(new Free(expr.temp));
		return co;
	}

	/**
	 * Generate a fresh temporary
	 * 
	 * @return new temporary register name
	 */
	protected String generateTemp(Scope.InnerType t) {
		switch(t) {
			case INT: 
			case PTR: //works the same for pointers
				return intTempPrefix + String.valueOf(++intRegCount);
			case FLOAT: return floatTempPrefix + String.valueOf(++floatRegCount);
			default: throw new Error("Generating temp for bad type");
		}
	}

	protected String generateLoopLabel() {
		return "loop_" + String.valueOf(++loopLabel);
	}

	protected String generateElseLabel() {
		return  "else_" + String.valueOf(++elseLabel);
	}

	protected String generateOutLabel() {
		return "out_" +  String.valueOf(++outLabel);
	}

	protected String generateFunctionLabel() {
		return "func_" + currFunc;
	}

	protected String generateFunctionLabel(String func) {
		return "func_" + func;
	}

	protected String generateFunctionOutLabel() {
		return "func_ret_" + currFunc;
	}
	
	/**
	 * Take a code object that results in an lval, and create a new code
	 * object that adds a load to generate the rval.
	 * 
	 * @param lco The code object resulting in an address
	 * @return A code object with all the code of <code>lco</code> followed by a load
	 *         to generate an rval
	 */
	protected CodeObject rvalify(CodeObject lco) {
		
		assert (lco.lval == true);
		CodeObject co = new CodeObject();

		/* FILL IN FROM STEP 2 */

		/* DON'T FORGET TO ADD CODE TO GENERATE LOADS FOR LOCAL VARIABLES */
		//Step 1: Add all the lco code to the new code object
	 	//	   (If lco is just a variable, create a new code object that
	 	//        stores the address of variable in a code object; see
	 	//        generateAddrFromVariable)
		if (lco.isVar()){
			// lco = generateAddrFromVariable(lco);
			InstructionList il = generateAddrFromVariable(lco);
			lco.temp = il.getLast().getDest();
			co.code.addAll(il);
		}
		else{
			co.code.addAll(lco.code);
		}
		co.ste = lco.ste;

		Instruction newtemp;
		if(lco.getType().type == Scope.InnerType.FLOAT){
			newtemp = new Flw(generateTemp(Scope.InnerType.FLOAT), lco.temp, "0");
		}
		else {
			newtemp = new Lw(generateTemp(Scope.InnerType.INT), lco.temp, "0");
		}

		co.code.add(newtemp);
		co.temp = newtemp.getDest();
		co.type = lco.getType();
		co.lval = false;

		return co;
	}

	/**
	 * Generate an instruction sequence that holds the address of the variable in a code object
	 * 
	 * If it's a global variable, just get the address from the symbol table
	 * 
	 * If it's a local variable, compute the address relative to the frame pointer (fp)
	 * 
	 * @param lco The code object holding a variable
	 * @return a list of instructions that puts the address of the variable in a register
	 */
	private InstructionList generateAddrFromVariable(CodeObject lco) {

		InstructionList il = new InstructionList();

		//Step 1:
		SymbolTableEntry symbol = lco.getSTE();
		String address = symbol.addressToString();

		//Step 2:
		Instruction compAddr = null;
		if (symbol.isLocal()) {
			//If local, address is offset
			//need to load fp + offset
			//addi tmp' fp offset
			compAddr = new Addi("fp", address, generateTemp(Scope.InnerType.INT));
		} else {
			//If global, address in symbol table is the right location
			//la tmp' addr //Register type needs to be an int
			compAddr = new La(generateTemp(Scope.InnerType.INT), address);
		}
		il.add(compAddr); //add instruction to code object

		return il;
	}

}
