require 'pegrb'
require_relative './ast'
require_relative './typingcontext'

module CMachineGrammar

  @grammar = Grammar.rules do

    ws = one_of(/\s/).many.any.ignore

    lpar, rpar = one_of('(').ignore, one_of(')').ignore

    comment = (m(';;') > one_of(/[^\r\n]/).many.any > one_of(/[\r\n]/).many).ignore

    symbol = (one_of(/[^\(\)\;]/)[:first] > one_of(/[^\s\(\)]/).many.any[:rest]) >> ->(s) {
      [s[:first], s[:rest]].flatten.map(&:text).join.to_sym
    }

    double_quote_string = (one_of('"') > one_of(/[^"]/).many[:chars] > one_of('"')) >> ->(s) {
      StringConst.new(s[:chars].map(&:text).join)
    }

    single_quote_string = (one_of("'") > one_of(/[^']/).many[:chars] > one_of("'")) >> ->(s) {
      StringConst.new(s[:chars].map(&:text).join)
    }

    string = double_quote_string | single_quote_string

    integer = one_of(/\d/).many[:digits] >> ->(s) {
      ConstExp.new(s[:digits].map(&:text).join.to_i)
    }

    boolean = (m('#t')[:true] | m('#f')[:false]) >> ->(s) {
      ConstExp.new(s[:true] ? true : false)
    }

    float = (one_of(/\d/).many[:integral] > one_of('.') > one_of(/\d/).many[:fractional]) >> ->(s) {
      ConstExp.new([s[:integral].map(&:text), '.', s[:fractional].map(&:text)].flatten.join.to_f)
    }

    atomic = float | integer | boolean | string | symbol

    empty_list = lpar > ws > rpar

    non_empty_list = (lpar > (ws > (atomic | r(:list))).many[:elements] > ws > rpar) >> ->(s) {
      s[:elements].map(&:first)
    }

    rule :list, non_empty_list | empty_list

    program = (ws > (comment | r(:list)) > ws).many

    rule :start, program[:p] >> ->(s) { s[:p].reject(&:empty?).map(&:first) }
  end

  ##
  # We need to recursively figure out what the type of the variable is because compound type
  # constructors like ptr and array can be nested arbitrarily.

  def self.type_resolution(type_specification)
    case type_specification
    when :void
      VoidType
    when :int
      IntType
    when :bool
      BoolType
    when :float
      FloatType
    when Array
      compound_type_wrapper = type_specification[0]
      wrapped_type = type_specification[1]
      wrapped_type_length = type_specification[2]
      case compound_type_wrapper
      when :ptr
        PtrType.new(type_resolution(wrapped_type))
      when :array
        ArrayType.new(type_resolution(wrapped_type), wrapped_type_length)
      else
        raise StandardError, "Unknown compound type specification."
      end
    else # non-basic type and not an array so must be a derived/declared type
      DerivedType.new(type_specification)
    end
  end

  @operator_map = {
   :'=' => EqExp, :< => LessExp, :+ => AddExp, :* => MultExp, :- => DiffExp, :/ => DivExp,
   :<= => LessEqExp, :> => GreaterExp
  }

  ##
  # Do some sanity checking and post-processing required to convert s-expression based syntax
  # to an actual tree of ast classes.

  def self.to_ast(s_expr)
    case s_expr
    when Symbol
      return SymbolWrapper.new(s_expr)
    when ConstExp
      return s_expr
    when StringConst
      return s_expr
    when Array
    else
      raise StandardError, "This should never happen: #{s_expr}."
    end

    # If we got to this point then we must be dealing with an array

    case (h = s_expr.first)
    when :struct # (struct name member type ... member type)
      struct_name = SymbolWrapper.new(s_expr[1])
      struct_members = s_expr[2..-1].each_slice(2).map do |member_name, member_type|
        StructMember.new(type_resolution(member_type), SymbolWrapper.new(member_name))
      end
      StructDeclaration.new(struct_name, struct_members)
    when :def # (def name (arg1 type ... argn type) return-type body)
      function_name = SymbolWrapper.new(s_expr[1])
      function_arguments = s_expr[2].each_slice(2).map do |argument_name, argument_type|
        ArgumentDefinition.new(type_resolution(argument_type), SymbolWrapper.new(argument_name))
      end
      return_type = type_resolution(s_expr[3])
      function_body = Statements.new(s_expr[4..-1].map {|e| to_ast(e)})
      # make sure the argument names are unique, could also be handled during type-checking
      if function_arguments.length != function_arguments.map(&:name).uniq.length
        raise StandardError, "Argument names must be unique: function = #{function_name}."
      end
      FunctionDefinition.new(return_type, function_name, function_arguments, function_body)
    when :do # (do stmt ... stmt)
      Statements.new(s_expr[1..-1].map {|e| to_ast(e)})
    when :declare # (declare name type value?)
      variable_name = SymbolWrapper.new(s_expr[1])
      variable_type = type_resolution(s_expr[2])
      variable_value = (value = s_expr[3]).nil? ? nil : to_ast(value)
      VariableDeclaration.new(variable_type, variable_name, variable_value)
    when :set # (set variable value)
      variable_name = to_ast(s_expr[1])
      variable_value = to_ast(s_expr[2])
      Assignment.new(variable_name, variable_value)
    when :nth # (nth array-reference index-expr)
      array_reference = to_ast(s_expr[1])
      index_expression = to_ast(s_expr[2])
      ArrayIndexExpression.new(array_reference, index_expression)
    when :if # (if test true-branch false-branch?)
      test = to_ast(s_expr[1])
      true_branch = to_ast(s_expr[2])
      false_branch = (false_code = s_expr[3]).nil? ? Statements.new([]) : to_ast(false_code)
      If.new(test, true_branch, false_branch)
    when :while # (while test body)
      test_expression = to_ast(s_expr[1])
      loop_body = to_ast(s_expr[2])
      While.new(test_expression, loop_body)
    when :for # (for init test update body)
      init = to_ast(s_expr[1])
      test = to_ast(s_expr[2])
      update = to_ast(s_expr[3])
      body = to_ast(s_expr[4])
      For.new(init, test, update, body)
    when :case # (case value case1 code ... casen code)
      unless (cases = s_expr[2..-2]).length % 2 == 0
        raise StandardError, "Case statement must have a default case."
      end
      case_element = to_ast(s_expr[1])
      default = to_ast(s_expr[-1])
      case_pairs = cases.each_slice(2).map do |element, expression|
        case element
        when ConstExp, StringConst
        else
          raise StandardError, "Case fragment must be constant: #{element}."
        end
        CaseFragment.new(element, to_ast(expression))
      end
      Switch.new(case_element, case_pairs, default)
    when :sizeof # (sizeof type-expression)
      type = type_resolution(s_expr[1])
      SizeOf.new(type)
    when :malloc # (malloc int-expression)
      size = to_ast(s_expr[1])
      Malloc.new(size)
    when :return # (return expression)
      return_expression = to_ast(s_expr[1])
      ReturnStatement.new(return_expression)
    when :+, :-, :*, :/, :>>, :<<, :^, :%, :&, :|
      elements = s_expr[1..-1].map {|e| to_ast(e)}
      @operator_map[h].new(elements)
    when :'=', :!=, :<, :>, :<=, :>=, :'&&', :'||'
      comparison_elements = s_expr[1..-1].map {|e| to_ast(e)}
      @operator_map[h].new(comparison_elements)
    else # (function arg arg ... arg)
      # If none of the above is true then it must be a function call
      function_arguments = s_expr[1..-1].map {|e| to_ast(e)}
      FunctionCall.new(SymbolWrapper.new(h), function_arguments)
    end
  end

  def self.annotate_types(ast)
    typing_context = TypingContext.new
    ast.each {|node| node.type_check(typing_context)}
  end

  def self.parse(iterable)
    s_expressions = @grammar.parse(iterable)
    ast = s_expressions.map {|l| to_ast(l)}
    annotate_types(ast)
  end

end
