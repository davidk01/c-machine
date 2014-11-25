require_relative './cmachine'
require_relative './compilecontext'

I = CMachine::Instruction

module CMachineGrammar

  ##
  # We use symbols for variables so instead of special casing symbols I'm just going to open
  # up the class and make sure symbols play nice with type checking and type inference.

  class ::Symbol

    def type_check(_)
      true
    end

    ##
    # Not much to do here other than look up the type of the variable in the typing context.

    def infer_type(typing_context)
      typing_context[self].type
    end

    ##
    # Get the variable data and push the contents on the stack.

    def compile(compile_context)
      variable_data = compile_context.get_variable_data(self)
      I[:loada, variable_data.offset, variable_data.declaration.type.size(compile_context)]
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

  class Identifier < Struct.new(:value)

    ##
    # An identifier means we are accessing a variable. So we load the value of the variable
    # on the stack through its address.

    def compile(compile_context)
      variable_data = compile_context.get_variable_data(value)
      raise StandardError, "Undefined variable access: #{val}." unless variable_data
      starting_address = variable_data.offset
      I[:loada, variable_data.offset, variable_data.size(compile_context)]
    end

  end

  class StringConst < Struct.new(:value)

    ##
    # Same as below.

    def compile(_); I[:loadc, value]; end

  end

  class ConstExp < Struct.new(:value)

    ##
    # What does it mean to typecheck a constant?

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
    # :loadc self.value

    def compile(_); I[:loadc, value]; end

  end

  class DiffExp < OpReducers

    ##
    # Same as for +AddExp+

    def infer_type(typing_context)
      return @type if @type
      expression_types = expressions.map {|e| e.infer_type(typing_context)}
      if !expression_types.all?(&:numeric?)
        raise StandardError, "Mis-typed subtraction expression."
      end
      @type = expression_types.reduce(&:upper_bound)
    end

    ##
    # For each expression in the list of expressions we compile it and then we append n - 1 :- operations,
    # where n is the length of the expressions. e1 e2 e3 ... en :- :- ... :-.

    def compile(compile_context); reduce_with_operation(compile_context, :-); end

  end

  class AddExp < OpReducers

    ##
    # To infer the type of an addition expression we infer the types of each addend and make sure
    # that each addend has an integer or float type. If there is a float type in the mix then
    # the type of the entire expression is float.

    def infer_type(typing_context)
      expression_types = expressions.map {|e| e.infer_type(typing_context)}
      if !expression_types.all?(&:numeric?)
        raise StandardError, "Mis-typed add expression."
      end
      @type ||= expression_types.reduce(&:upper_bound)
    end

    ##
    # Same reasoning as for +DiffExp+ except we use :+.

    def compile(compile_context); reduce_with_operation(compile_context, :+); end

  end

  class ModExp < OpReducers

    ##
    # Might need to re-think this since repeated modulo operation doesn't make much sense.
    
    def compile(compile_context); reduce_with_operation(compile_context, :%); end

  end

  class LeftShift < OpReducers

    ##
    # Same as above.

    def compile(compile_context); reduce_with_operation(compile_context, :<<); end

  end

  class RightShift < OpReducers

    ##
    # Same as above.

    def compile(compile_context); reduce_with_operation(compile_context, :>>); end

  end

  class MultExp < OpReducers

    def type_check(typing_context)
      expressions.each {|e| e.type_check(typing_context)}
      expression_types = expressions.map {|e| e.infer_type(typing_context)}
      if !expression_types.all?(&:numeric?)
        raise StandardError, "Can not multiply non-numeric arguments."
      end
    end

    ##
    # Same as for +AddExp+.

    def infer_type(typing_context)
      expression_types = expressions.map {|e| e.infer_type(typing_context)}
      if !expression_types.all?(&:numeric?)
        raise StandardError, "Mis-typed multiplication expression."
      end
      @type ||= expression_types.reduce(&:upper_bound)
    end

    ##
    # Same as above.

    def compile(compile_context); reduce_with_operation(compile_context, :*); end

  end

  class DivExp < OpReducers

    ##
    # Same as for +AddExp+.

    def infer_type(typing_context)
      expression_types = expressions.map {|e| e.infer_type(typing_context)}
      if !expression_types.all?(&:numeric?)
        raise StandardError, "Mis-typed division expression."
      end
      @type ||= expression_types.reduce(&:upper_bound)
    end

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

    ##
    # e1 e2 :< e2 e3 :< e3 e4 :< ... en-1 en :< :& :& ... :&

    def compile(compile_context); reduce_with_comparison(compile_context, :<); end

  end

  class LessEqExp < OpReducers

    def type_check(typing_context)
      expressions.each {|e| e.type_check(typing_context)}
      expression_types = expressions.map {|e| e.infer_type(typing_context)}
      if !expression_types.all?(&:numeric?)
        raise StandardError, "Non numeric type found for ordering comparison."
      end
    end

    ##
    # We can only compare numeric things.

    def infer_type(typing_context)
      expression_types = expressions.map {|e| e.infer_type(typing_context)}
      if [IntType, FloatType].include?(expression_types.reduce(:upper_bound))
        BoolType
      else
        raise StandardError, "Can not order non-numeric types."
      end
    end

    ##
    # Same as above.

    def compile(compile_context); reduce_with_comparison(compile_context, :<=); end

  end

  class EqExp < OpReducers

    ##
    # Same as above.

    def compile(compile_context); reduce_with_comparison(compile_context, :==); end

  end

  class GreaterExp < OpReducers

    ##
    # Same as above.

    def compile(compile_context); reduce_with_comparison(compile_context, :>); end

  end

  class GreaterEqExp < OpReducers

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

    ##
    # The expression is already a type expression so we just need to look it up in the
    # context and replace it with the size.

    def compile(compile_context)
      raise StandardError, "Not implemented."
    end

  end

  class Malloc < Struct.new(:size)

    ##
    # Malloc is special when it comes to types. It is a wildcard pointer and so can be assigned
    # to any pointer type variable.

    def infer_type(_)
      WildcardPointer
    end

    ##
    # Allocate the requested amount of space and return a pointer to the start of the allocated
    # memory block.

    def compile(compile_context)
      raise StandardError, "Not implemented."
    end

  end

  class Assignment < Struct.new(:left, :right)

    ##
    # The type of the left expression and the type of the right expression have to be
    # conformant. I'm not sure what this means though because there is an lvalue and rvalue
    # distinction that is still unclear to me.

    def type_check(typing_context)
      if left.infer_type(typing_context) != right.infer_type(typing_context)
        raise StandardError, "Non-conformant assignment."
      end
    end

    ##
    # The types of left and right need to match and the left side needs to be an lvalue.

    def compile(compile_context)
      raise StandardError, "Not implemented."
    end

  end

  class If < Struct.new(:test, :true_branch, :false_branch)

    ##
    # The test must be boolean and then each branch must be well-typed. I don't see any reason
    # why both branches must be of the same type so I'm not going to force that condition. Will
    # revisit later.

    def type_check(typing_context)
      test.type_check(typing_context)
      if test.infer_type(typing_context) != BoolType
        raise StandardError, "Test for if statement must be of boolean type."
      end
      true_branch.type_check(typing_context)
      false_branch.type_check(typing_context)
    end

    ##
    # test jumpz(:else) true_branch jump(:end) [:else] false_branch [:end]
    # Jump targets are in terms of symbolic labels which later become actual addresses
    # during a post-processing step. The labels themselves stay in the code but the VM
    # interprets them as no-ops.

    def compile(compile_context)
      else_target, end_target = compile_context.get_label, compile_context.get_label
      test.compile(compile_context) + I[:jumpz, else_target] +
       true_branch.compile(compile_context.increment) + I[:jump, end_target] + I[:label, else_target] +
       false_branch.compile(compile_context.increment) + I[:label, end_target]
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
       body.compile(compile_context) + I[:jump, test_target]
    end

  end

  class For < Struct.new(:init, :test, :update, :body)

    ##
    # For loop for(e1;e2;e3;) { s } is equivalent to e1; while (e2) { s; e3; } so we compile it as
    # init [:test] test jumpz(:end) body update jump(:test) [:end]
    # A bit convoluted but manageable.

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
    # +Statements+ instances result from a block so we need to introduce a new context
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

  class ExpressionStatement < Struct.new(:expression)

    ##
    # Pretty simple. Compile the expression and then pop.
    # I'm not sure if a single pop is enough. What happens when we load
    # a compound variable on top of the stack? (TODO: Currently the stack invariant
    # is not maintained so need to figure out the correct number of elements to pop)

    def compile(compile_context)
      expression.compile(compile_context) + I[:pop, 1]
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

    ##
    # Size of an array is exactly what you'd expect it to be.

    def size(_); count.value * type.size(_); end

  end

  class PtrType < Struct.new(:type)

    ##
    # Pointers are just integers and in our VM scheme they take up just 1 memory cell.

    def size(_); 1; end

  end

  ##
  # Derived types.

  class DerivedType < Struct.new(:name)

    ##
    # To figure out the size of a derived type we first have to look it up in the compilation
    # context and return the size of whatever struct was declared by that name.

    def size(compile_context); compile_context.get_struct_data(name).size(compile_context); end

  end

  class StructMember < Struct.new(:type, :name)

    ##
    # Size of a struct member is the size of the underlying type.

    def size(compile_context); @size ||= type.size(compile_context); end

  end

  class StructDeclaration < Struct.new(:name, :members)

    ##
    # Type checking a struct declaration is pretty simple because we just add information
    # to the typing context.

    def type_check(typing_context)
      typing_context[name] = self
    end

    ##
    # The offset for the struct members is exactly what you'd expect. It is the sum of all
    # the members that are declared before that member and this information is computed when
    # we compute the total size of the struct.
    
    def offset(compile_context, member)
      # Call size to instantiate +@offsets+ hash and then lookup the member in the hash.
      size(compile_context)
      if (member_offset = @offsets[member]).nil?
        raise StandardError, "Unknown struct member #{member} for struct #{name}."
      end
      member_offset
    end

    ##
    # The size of a declared struct is the sum of the sizes of all its members and as
    # we are computing the size of the entire struct we can also compute and save the offsets
    # for the struct members in +@offsets+.

    def size(compile_context)
      @offsets ||= {}
      @size ||= members.reduce(0) do |m, member|
        @offsets[member.name] = m
        m + member.size(compile_context)
      end
    end

    ##
    # Compiling struct declarations means putting information in a symbol table for the given struct.

    def compile(compile_context)
      if !compile_context.get_struct_data(name).nil?
        raise StandardError, "A struct by the given name is already defined: #{name}."
      end
      compile_context.assign_struct_data(name, self)
      []
    end

  end

  ##

  class VariableDeclaration < Struct.new(:type, :variable, :value)

    ##
    # First we need to make sure a variable or function with the same name has not already
    # been declared. Then if there is a value we need to make sure that the type of the initializer
    # matches the type of the variable.

    def type_check(typing_context)
      if typing_context[variable]
        raise StandardError, "Can not declare two variables with the same name: #{variable}."
      end
      typing_context[variable] = self
      if value
        if (value_type = value.infer_type(typing_context)) != type
          raise StandardError, "Type of variable does not match type of initializer: #{variable}."
        end
      end
    end

    ##
    # TODO: Get this to work.

    def compile(compile_context)
      variable_initialization = I[:initvar, @size]
      variable_assignment = value ?
       value.compile(compile_context) +
       I[:storea, @offset, (s = value.size(compile_context))] + I[:pop, s] : []
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
      typing_context.current_function = self
      arguments.each {|arg| arg.type_check(typing_context) && typing_context[arg.name] = arg}
      @arguments_size = arguments.map {|arg| arg.type.size(typing_context)}.reduce(&:+)
      body.type_check(typing_context)
    end

    def compile(compile_context)
      function_context = compile_context.increment
      I[:label, name] + body.compile(function_context)
    end

  end

  ##
  # We treat argument definitions as phantom variable declarations during compilation.
  # What I mean by phantom is that we update the compilation context but we do not output
  # any actual bytecode.

  class ArgumentDefinition < Struct.new(:type, :name)

  end

  ##
  # Notes: No idea how this is supposed to work either. Trying to avoid frame pointers seems
  # like a lot of hassle.

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
      @arguments_size ||= function_argument_types.reduce(0) {|m, t| m + t.size(typing_context)}
      call_argument_types = arguments.map {|arg| arg.infer_type(typing_context)}
      if !function_argument_types.zip(call_argument_types).all? {|a, b| a == b}
        raise StandardError, "Call site arguments do not match definition: #{name}."
      end
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
       I[:pushstack, @arguments_size] + I[:call, name]
    end

  end

end
