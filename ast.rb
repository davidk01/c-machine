require_relative './cmachine'
require_relative './compilecontext'

I = CMachine::Instruction

module CMachineGrammar

  class SymbolWrapper < Struct.new(:symbol)

    attr_reader :size, :offset

    def to_s
      "#{symbol}"
    end

    def infer_type(typing_context)
      typing_context[self].type
    end

    ##
    # Get the offset and size from the typing context.

    def type_check(typing_context)
      declaration = typing_context[self]
      @offset, @size = declaration.offset, declaration.size
    end

    def compile(compile_context)
      I[:loada, @offset, @size]
    end

    def lvalue(compile_context)
      I[:loadc, @offset]
    end

  end

  ##
  # Arithmetic and order operations have a common structure when it comes to compiling them
  # to stack operations. +OpReducers+ encapsulates that commonality.

  class OpReducers < Struct.new(:expressions)

    ##
    # e.g. :+, :-, :/, :*

    def reduce_with_operation(compile_context, operation)
      expr = expressions.map {|e| e.compile(compile_context)}
      expr[0] + expr[1..-1].map {|e| e + I[operation]}.flatten
    end

    ##
    # e.g. :<, :==, :>, etc.

    def reduce_with_comparison(compile_context, comparison)
      exprs = expressions.map {|e| e.compile(compile_context)}
      comparisons = exprs[0...-1].zip(exprs[1..-1]).map {|a, b| a + b + I[comparison]}.flatten
      comparisons + I[:&] * (expressions.length - 2)
    end

  end

  # AST classes

  class StringConst < Struct.new(:value)

    ##
    # Same as below.

    def compile(_); I[:loadc, value]; end

  end

  class ConstExp < Struct.new(:value)

    def to_s
      "#{value}"
    end

    ##
    # TODO: Don't know.

    def type_check(typing_context)
      true
    end

    ##
    # We just need to compare the constant to our basic constants and return the proper type.

    def infer_type(typing_context)
      @type ||= case value
        when Float
          FloatType
        when Integer
          IntType
        when true, false
          BoolType
        else
          raise StandardError, "Unknown type for constant: #{value}."
        end
    end

    ##
    # Just load the constant.

    def compile(_); I[:loadc, value]; end

  end

  module NumericTypeHelpers

    def infer_type(typing_context)
      return @type if @type
      expression_types = expressions.map {|e| e.infer_type(typing_context)}
      if !expression_types.all?(&:numeric?)
        raise StandardError, "Mis-typed subtraction expression."
      end
      @type = expression_types.reduce(&:upper_bound)
    end

    def type_check(typing_context)
      expressions.each {|e| e.type_check(typing_context)}
      expression_types = expressions.map {|e| e.infer_type(typing_context)}
      if !expression_types.all?(&:numeric?)
        raise StandardError, "Can not multiply non-numeric arguments."
      end
    end

  end

  class DiffExp < OpReducers

    include NumericTypeHelpers

    ##
    # For each expression in the list of expressions we compile it and then we append n - 1 :- operations,
    # where n is the length of the expressions. e1 e2 e3 ... en :- :- ... :-.

    def compile(compile_context); reduce_with_operation(compile_context, :-); end

  end

  class AddExp < OpReducers

    include NumericTypeHelpers

    ##
    # Same reasoning as for +DiffExp+ except we use :+.

    def compile(compile_context); reduce_with_operation(compile_context, :+); end

  end

  class ModExp < OpReducers

    include NumericTypeHelpers

    ##
    # Might need to re-think this since repeated modulo operation doesn't make much sense.
    
    def compile(compile_context); reduce_with_operation(compile_context, :%); end

  end

  class LeftShift < OpReducers

    include NumericTypeHelpers

    ##
    # Same as above.

    def compile(compile_context); reduce_with_operation(compile_context, :<<); end

  end

  class RightShift < OpReducers

    include NumericTypeHelpers

    ##
    # Same as above.

    def compile(compile_context); reduce_with_operation(compile_context, :>>); end

  end

  class MultExp < OpReducers

    include NumericTypeHelpers

    ##
    # Same as above.

    def compile(compile_context); reduce_with_operation(compile_context, :*); end

  end

  class DivExp < OpReducers

    include NumericTypeHelpers

    ##
    # Same as above.

    def compile(compile_context); reduce_with_operation(compile_context, :/); end

  end

  class AndExp < OpReducers

    def compile(compile_context); reduce_with_operation(compile_context, :&); end

  end

  class OrExp < OpReducers

    def compile(compile_context); reduce_with_operation(compile_context, :|); end

  end

  class LessExp < OpReducers

    def infer_type(typing_context)
      BoolType
    end

    def type_check(typing_context)
      expressions.each {|e| e.type_check(typing_context)}
      expression_types = expressions.map {|e| e.infer_type(typing_context)}
      if !expression_types.all?(&:numeric?)
        raise StandardError, "Can not compare non-numeric arguments."
      end
    end

    ##
    # e1 e2 :< e2 e3 :< e3 e4 :< ... en-1 en :< :& :& ... :&

    def compile(compile_context); reduce_with_comparison(compile_context, :<); end

  end

  class LessEqExp < OpReducers

    def infer_type(typing_context)
      BoolType
    end

    def type_check(typing_context)
      expressions.each {|e| e.type_check(typing_context)}
      expression_types = expressions.map {|e| e.infer_type(typing_context)}
      if !expression_types.all?(&:numeric?)
        raise StandardError, "Can not compare non-numeric arguments."
      end
    end

    ##
    # Same as above.

    def compile(compile_context); reduce_with_comparison(compile_context, :<=); end

  end

  class EqExp < OpReducers

    def infer_type(typing_context)
      BoolType
    end

    def type_check(typing_context)
      expressions.each {|e| e.type_check(typing_context)}
      expression_types = expressions.map {|e| e.infer_type(typing_context)}
      if !expression_types.all?(&:numeric?)
        raise StandardError, "Can not compare non-numeric arguments."
      end
    end

    ##
    # Same as above.

    def compile(compile_context); reduce_with_comparison(compile_context, :==); end

  end

  class GreaterExp < OpReducers

    def infer_type(typing_context)
      BoolType
    end

    def type_check(typing_context)
      expressions.each {|e| e.type_check(typing_context)}
      types = expressions.map {|e| e.infer_type(typing_context)}
      if !types.all?(&:numeric?)
        raise StandardError, "Can not compare non-numeric types."
      end
    end

    ##
    # Same as above.

    def compile(compile_context); reduce_with_comparison(compile_context, :>); end

  end

  class GreaterEqExp < OpReducers

    def infer_type(typing_context)
      BoolType
    end

    def type_check(typing_context)
      expressions.each {|e| e.type_check(typing_context)}
      expression_types = expressions.map {|e| e.infer_type(typing_context)}
      if !expression_types.all?(&:numeric?)
        raise StandardError, "Can not compare non-numeric arguments."
      end
    end

    ##
    # Same as above.

    def compile(compile_context); reduce_with_comparison(compile_context, :>=); end

  end

  class NotExp < Struct.new(:expression)

    ##
    # e :!

    def compile(compile_context); expression.compile(compile_context) + I[:!]; end

  end

  class NegExp < Struct.new(:expression)

    ##
    # e :-@

    def compile(compile_context); expression.compile(compile_context) + I[:-@]; end

  end

  class SizeOf < Struct.new(:type)

    def type_check(typing_context)
      @size = type.size(typing_context)
    end

    def infer_type(typing_context)
      IntType
    end

    def compile(compile_context)
      I[:loadc, @size]
    end

  end

  class Malloc < Struct.new(:size)

    def type_check(typing_context)
      size.type_check(typing_context)
      if size.infer_type(typing_context) != IntType
        raise StandardError, "Can not allocate non-integer amount of space."
      end
    end

    def infer_type(_)
      WildcardPointer
    end

    def compile(compile_context)
      size.compile(compile_context) + I[:malloc]
    end

  end

  class StructMemberAccess < Struct.new(:reference, :member)

    def to_s
      "#{reference}.#{member}"
    end

    ##
    # TODO: A whole bunch of stuff.

    def type_check(typing_context)
      reference.type_check(typing_context)
      if !(DerivedType === (reference_type = reference.infer_type(typing_context)))
        raise StandardError, "Can only access members of struct types."
      end
      if !(SymbolWrapper === member)
        raise StandardError, "Struct member accessor must be a name."
      end
      if (@member_offset = reference_type.offset(member)).nil?
        raise StandardError, "Member undefined for given struct: #{member}."
      end
      @member_size = reference_type.member_type(member).size(typing_context)
      true
    end

    def infer_type(typing_context)
      reference.infer_type(typing_context).member_type(member)
    end

    def lvalue(compile_context)
      reference.lvalue(compile_context) + I[:loadc, @member_offset] + I[:+]
    end

    def compile(compile_context)
      reference.lvalue(compile_context) + I[:loadc, @member_offset] + I[:+] +
       I[:load, @member_size]
    end

  end

  class ArrayIndexExpression < Struct.new(:reference, :index)

    def type_check(typing_context)
      reference.type_check(typing_context)
      index.type_check(typing_context)
      if index.infer_type(typing_context) != IntType
        raise StandardError, "Array index must be integer."
      end
      if !(ArrayType === (array_type = reference.infer_type(typing_context)))
        raise StandardError, "Can not index non-array reference type."
      end
      @index_scale = array_type.type.size(typing_context)
    end

    def infer_type(typing_context)
      reference.infer_type(typing_context).type
    end

    def lvalue(compile_context)
      reference.lvalue(compile_context) + I[:loadc, @index_scale] +
       index.compile(compile_context) + I[:*] + I[:+]
    end

    def compile(compile_context)
      reference.lvalue(compile_context) + I[:loadc, @index_scale] +
       index.compile(compile_context) + I[:*] + I[:+] + I[:load, @index_scale]
    end

  end

  class Assignment < Struct.new(:left, :right)

    ##
    # The type of the left expression and the type of the right expression have to be
    # conformant. I'm not sure what this means though because there is an lvalue and rvalue
    # distinction that is still unclear to me.

    def type_check(typing_context)
      left.type_check(typing_context)
      right.type_check(typing_context)
      if (left_type = left.infer_type(typing_context)) != (right_type = right.infer_type(typing_context))
        raise StandardError, "Non-conformant assignment: left = #{left}, right = #{right}, left type = #{left_type}, right type = #{right_type}."
      end
      @size = right.infer_type(typing_context).size(typing_context)
    end

    ##
    # The types of left and right need to match and the left side needs to be an lvalue.

    def compile(compile_context)
      right.compile(compile_context) + left.lvalue(compile_context) + I[:store, @size] +
       I[:pop, @size]
    end

  end

  class If < Struct.new(:test, :true_branch, :false_branch)

    def type_check(typing_context)
      test.type_check(typing_context)
      if test.infer_type(typing_context) != BoolType
        raise StandardError, "Test for if statement must be of boolean type."
      end
      # true and false branches are treated as separate blocks
      true_branch.type_check(typing_context.increment)
      false_branch.type_check(typing_context.increment)
    end

    ##
    # test jumpz(:else) true_branch jump(:end) [:else] false_branch [:end]
    # Jump targets are in terms of symbolic labels which later become actual addresses
    # during a post-processing step. The labels themselves stay in the code but the VM
    # interprets them as no-ops.

    def compile(compile_context)
      else_target, end_target = compile_context.get_label, compile_context.get_label
      test.compile(compile_context) + I[:jumpz, else_target] +
       true_branch.compile(compile_context.increment) + I[:jump, end_target] +
       I[:label, else_target] + false_branch.compile(compile_context.increment) +
       I[:label, end_target]
    end

  end

  class While < Struct.new(:test, :body)

    ##
    # [:test] test jumpz(:end) body jump(:test) [:end]
    # Pretty similar to how we compile "if" statements. We have some jump targets and a test
    # to figure out where to jump.

    def compile(compile_context)
      test_target, end_target = compile_context.get_label, compile_context.get_label
      I[:label, test_target] + test.compile(compile_context) + I[:jumpz, end_target] +
       body.compile(compile_context) + I[:jump, test_target] + I[:label, end_target]
    end

    def type_check(typing_context)
      test.type_check(typing_context)
      body.type_check(typing_context)
      if !test.infer_type(typing_context) == BoolType
        raise StandardError, "Mis-typed condition expression for while loop."
      end
    end

  end

  class For < Struct.new(:init, :test, :update, :body)

    ##
    # For loop for(e1;e2;e3;) { s } is equivalent to e1; while (e2) { s; e3; } so we compile it as
    # init [:test] test jumpz(:end) body update jump(:test) [:end]
    # TODO: Fix this because it is broken, specifically I[:pop, 1]

    def compile(compile_context)
      test_target, end_target = compile_context.get_label, compile_context.get_label
      init.compile(compile_context) + I[:label, test_target] +
       test.expression.compile(compile_context) + I[:jumpz, end_target] +
       body.compile(compile_context) + update.compile(compile_context) + I[:pop, 1] +
       I[:jump, test_target] + I[:label, end_target]
    end

  end

  class CaseFragment < Struct.new(:case, :body)

    ##
    # Just compile the body. The rest is taken care of by the parent node
    # which should always be a +Switch+ node.

    def compile(compile_context); body.compile(compile_context); end

  end

  class Switch < Struct.new(:test, :cases, :default)

    ##
    # Assume the cases are sorted then generating the code is pretty simple.
    # The base case is less than 3 case values. For less than 3 values we generate
    # a simple comparison ladder. For the non-base case we proceed recursively by
    # breaking things into middle, top, bottom and then concatenating the generated code
    # with appropriate jump targets when the case matches and jumps to the other pieces of
    # the ladder when it doesn't.

    def generate_binary_search_code(cases, labels, compile_context)
      if cases.length < 3
        cases.map do |c|
          I[:dup] + I[:loadc, (c_val = c.case.value)] + I[:==] + I[:jumpnz, labels[c_val]]
        end.flatten
      else
        # mid top :less bottom
        midpoint, less_label = cases.length / 2, compile_context.get_label
        middle, bottom, top = cases[midpoint], cases[0...midpoint], cases[(midpoint + 1)..-1]
        I[:dup] + I[:loadc, (m_val = middle.case.value)] + I[:==] + I[:jumpnz, labels[m_val]] +
         I[:dup] + I[:loadc, m_val] + I[:<] + I[:jumpnz, less_label] +
         generate_binary_search_code(top, labels, compile_context) + I[:label, less_label] +
         generate_binary_search_code(bottom, labels, compile_context)
      end
    end

    ##
    # We are going to use binary search to figure out which case statement to jump to.
    # First we sort the case statements and assign jump targets to each statement. Then we
    # generate the binary search ladder for the cases and place it before the case blocks.

    def compile(compile_context)
      # Sort and generate labels for the cases.
      default_label = compile_context.get_label
      sorted_cases = cases.sort {|a, b| a.case.value <=> b.case.value}
      labels = sorted_cases.reduce({}) {|m, c| m[c.case.value] = compile_context.get_label; m}
      # Generate the binary search ladder.
      binary_search_sequence = generate_binary_search_code(sorted_cases, labels, compile_context)
      # Compile the test expression, attach the binary search ladder and the code for each case.
      (test.compile(compile_context) + binary_search_sequence + I[:jump, default_label] +
       sorted_cases.map {|c| I[:label, labels[c.case.value]] + c.compile(compile_context)} +
       I[:label, default_label] + default.compile(compile_context)).flatten
    end

  end

  class Statements < Struct.new(:statements)

    ##
    # +Statements+ instances correspond to lexical blocks so we need to introduce a new context
    # that corresponds to the new block.

    def type_check(typing_context)
      statements_context = typing_context.increment
      statements.each {|s| s.type_check(statements_context)}
    end

    ##
    # s1 pop s2 pop s3 pop ... sn pop

    def compile(compile_context)
      statements.map {|s| s.compile(compile_context)}.flatten
    end

  end

  # Type related nodes.
  ##

  class BaseType
    def self.size(_); 1; end
  end

  ##
  # Common operations for numeric types.

  module NumericType

    def numeric?
      true
    end

    def upper_bound(type)
      if type == FloatType
        FloatType
      elsif IntType
        self == IntType ? IntType : FloatType
      else
        raise StandardError, "Non numeric type: #{type}."
      end
    end

  end

  class IntType < BaseType

    def to_s
      "Int"
    end

    # TODO: Don't know.

    def self.type_check(typing_context)
      true
    end

    extend NumericType

  end

  class FloatType < BaseType

    extend NumericType

  end

  class BoolType < BaseType; end

  class VoidType < BaseType
    def self.size(_); 0; end
  end

  ##
  # This is the type that malloc returns and it conforms to any kind of pointer type.

  class WildcardPointer
  
    def self.==(other_type)
      PtrType === other_type
    end

  end

  # Semi-base types.
  class ArrayType < Struct.new(:type, :count)

    def to_s
      "array(#{type}, #{count})"
    end

    # TODO: Figure out if this is actually correct.

    def type_check(typing_context)
      if !(Integer === (c = count.value) && c > 0)
        raise StandardError, "Array size must be a positive integer."
      end
      type.type_check(typing_context)
    end

    ##
    # Size of an array is exactly what you'd expect it to be.

    def size(_); count.value * type.size(_); end

  end

  class PtrType < Struct.new(:type)

    def to_s
      "ptr(#{type})"
    end

    ##
    # TODO: What does it mean to type check a pointer type.

    def type_check(typing_context)
      true
    end

    ##
    # Pointers are just integers and in our VM scheme they take up just 1 memory cell.

    def size(_); 1; end

  end

  ##
  # Derived types.

  class DerivedType < Struct.new(:name)

    def to_s
      name.symbol
    end

    ##
    # All derived types must resolve to a struct declaration.

    def type_check(typing_context)
      if (@struct = typing_context[name]).nil?
        raise StandardError, "Type has not been declared in this context."
      end
      true
    end

    def member_type(member)
      @struct.member_type(member)
    end

    def offset(member)
      @struct.offset(member)
    end

    ##
    # To figure out the size of a derived type we first have to look it up in the compilation
    # context and return the size of whatever struct was declared by that name.

    def size(typing_context)
      @struct.size(typing_context)
    end

  end

  class PointerDereference < Struct.new(:pointer_reference)

    ##
    # Make sure we are actually dealing with a pointer.

    def type_check(typing_context)
      pointer_reference.type_check(typing_context)
      if !(PtrType === (@ptr_type = pointer_reference.infer_type(typing_context)))
        raise StandardError, "Can not de-reference non-pointer type."
      end
      @pointer_value_size = @ptr_type.type.size(typing_context)
    end

    def infer_type(typing_context)
      @ptr_type.type
    end

    ##
    # TODO: Figure this out.

    def lvalue(compile_context)
      pointer_reference.compile(compile_context)
    end

    def compile(compile_context)
      pointer_reference.compile(compile_context) + I[:load, @pointer_value_size]
    end

  end

  class StructMember < Struct.new(:type, :name)

    ##
    # TODO: Don't know.

    def type_check(typing_context)
      type.type_check(typing_context)
    end

    def size(typing_context); @size ||= type.size(typing_context); end

  end

  class StructDeclaration < Struct.new(:name, :members)

    ##
    # Nothing to do.

    def compile(compile_context)
      []
    end

    ##
    # Calculate size and offsets for members and also make sure member names are unique and that
    # a struct by the same name has not already been declared.

    def type_check(typing_context)
      if (member_names = members.map(&:name).map(&:symbol)).length != member_names.uniq.length
        raise StandardError, "Member names must be unique: #{member_names}."
      end
      if typing_context[name]
        raise StandardError, "A struct by that name already exists: #{name}."
      end
      typing_context[name] = self
      members.each {|m| m.type_check(typing_context)}
      @offsets, @member_types = {}, {}
      @size = members.reduce(0) do |m, member|
        @offsets[member.name] = m
        @member_types[member.name] = member.type
        m + member.type.size(typing_context)
      end
    end

    def member_type(member)
      @member_types[member]
    end

    def offset(member)
      @offsets[member]
    end

    def size(typing_context)
      @size
    end

  end

  ##

  class VariableDeclaration < Struct.new(:type, :variable, :value)

    attr_reader :size, :offset

    def type_check(typing_context)
      type.type_check(typing_context)
      value.type_check(typing_context) if value
      if typing_context[variable]
        require 'pry'; binding.pry
        raise StandardError, "Can not declare two variables with the same name: #{variable}."
      end
      typing_context[variable] = self
      if value
        if (value_type = value.infer_type(typing_context)) != type
          raise StandardError, "Type of variable does not match type of initializer: #{variable}."
        end
      end
      latest_declaration = typing_context.latest_declaration
      @size, @offset = type.size(typing_context), latest_declaration.offset + latest_declaration.size
      typing_context.latest_declaration = self
      variable.type_check(typing_context)
    end

    def compile(compile_context)
      variable_initialization = I[:initvar, @size]
      variable_assignment = value ?
       value.compile(compile_context) +
       I[:storea, @offset, @size] + I[:pop, @size] : []
      variable_initialization + variable_assignment
    end

  end

  ##
  # Generate a label that marks the beginning of the function so that function calls can
  # jump to the code.
  # Notes: The arguments are already meant to be on the stack set up by whoever has called us
  # this means I need to augment the context and treat each argument as a variable declaration.

  class FunctionDefinition < Struct.new(:return_type, :name, :arguments, :body)

    attr_reader :arguments_size

    ##
    # Extend the context with function arguments, set up some bookkeeping information like
    # which function we are currently working in and then typecheck the body.

    def type_check(typing_context)
      typing_context[name] = self
      function_context = typing_context.increment
      function_context.current_function = self
      offset = 0
      arguments.each do |arg|
        arg.type_check(function_context)
        function_context[arg.name] = arg
        arg.offset = offset
        arg.size = arg.type.size(function_context)
        offset += arg.size
      end
      @arguments_size = arguments.map {|arg| arg.type.size(function_context)}.reduce(&:+)
      body.type_check(function_context)
    end

    def compile(compile_context)
      function_context = compile_context.increment
      I[:label, name.symbol] + body.compile(function_context)
    end

  end

  class ArgumentDefinition < Struct.new(:type, :name, :offset, :size)

    ##
    # TODO: Figure out what it means to typecheck an argument definition.

    def type_check(typing_context)
      typing_context.latest_declaration = self
      type.type_check(typing_context)
      if !(SymbolWrapper === name)
        raise StandardError, "Argument name must be a symbol: #{name}."
      end
    end

  end

  class Void

    def self.compile(compile_context)
      []
    end

    def self.infer_type(typing_context)
      VoidType
    end

    def self.type_check(typing_context)
      true
    end

  end

  class ReturnStatement < Struct.new(:return_expression)
  
    ##
    # Verify that the return expression and the current function have the same type. Also, stash
    # away the size of the return type because we are going to need it during compilation.

    def type_check(typing_context)
      return_expression.type_check(typing_context)
      current_function = typing_context.current_function
      return_type = return_expression.infer_type(typing_context)
      if return_type != current_function.return_type
        raise StandardError, "Type of return expression #{return_expression} does not match type of function: #{current_function.name}."
      end
      @return_size = current_function.return_type.size(typing_context)
    end

    def compile(compile_context)
      return_expression.compile(compile_context) +
       I[:storea, 0, @return_size] +
       I[:return, @return_size]
    end

  end

  ##
  # TODO: Write explanation.

  class FunctionCall < Struct.new(:name, :arguments)

    ##
    # The arguments at the call site must match the signature of the function being called.

    def type_check(typing_context)
      function = typing_context[name]
      function_argument_types = function.arguments.map(&:type)
      arguments.each {|arg| arg.type_check(typing_context)}
      call_argument_types = arguments.map {|arg| arg.infer_type(typing_context)}
      function_argument_types.zip(call_argument_types).each_with_index do |(a, b), i|
        if !(a == b)
          raise StandardError, "Call site arguments do not match definition at index #{i}: #{name}, decl type = #{a.to_s}, call types = #{b}."
        end
      end
      @arguments_size = function.arguments_size
    end

    ##
    # Pretty obvious. When calling a function the type is whatever is returned from the function
    # and since the return type is part of the signature we just look at the return type for
    # the function signature.

    def infer_type(typing_context)
      @type ||= typing_context[name].return_type
    end

    def compile(compile_context)
      # Evaluate the arguments and then transport them to the new stack
      arguments.flat_map {|arg| arg.compile(compile_context)} +
       I[:pushstack, @arguments_size] + I[:call, name.symbol]
    end

  end

end
