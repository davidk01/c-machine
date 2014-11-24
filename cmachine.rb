require_relative './cmachinestack'

##
# The actual virtual machine class.

class CMachine

  ##
  # It is important to distinguish between absolute loads and stores and relative loads and
  # stores.

  ##
  # Need a convenient way to see all the instructions. Simple comment for the time being
  # :label l (symbolic labels for jump instructions)
  # :pop (decrement the stack pointer)
  # :pushstack k (add another stack on top of the current one for a function call and move k values
  #               from the current stack to the new one)
  # :call label (call a function by jumping to the given label/address after saving @pc)
  # :return (jump back to a saved @pc)
  # :loadc c (push a constant on top of the stack)
  # :load c (take the top of the stack as a starting address and load c values from it)
  # :store c (take the top of the stack as a starting address and store c values to it)
  # :loada s c (load c elements starting at s)
  # :storea s c (store c elements starting at s)
  # :initvar s l (initialize l zeroes starting at s)
  # :jump a (jump to address a)
  # :jumpz a (jump to address a if top of stack is 0)
  # :jumpnz a (jump to address a if top of stack is not 0)
  # :jumpi a (indexed jump takes top of stack and adds a to it and jumps to that address)
  # :dup (duplicate top value on stack)
  # :*, :/, :+, :-, :% (arithmetic instructions)
  # :==, :!=, :<, :<=, :>, :>= (comparison instructions)
  # :-@, :! (unary instructions)
  # :&, :| (boolean/bitwise instructions)

  ##
  # An instruction is just a symbol along with any necessary arguments.
  # E.g. +Instruction.new(:loadc, [1])+

  class Instruction < Struct.new(:instruction, :arguments)
    def self.[](instruction, *arguments); [new(instruction, arguments)]; end
  end

  ##
  # Readers for most of the internal state. Will help with debugging.

  attr_reader :code, :stack, :pc, :ir, :return

  ##
  # Set up the initial stack and registers.
  
  def initialize(c)
    @code, @stack, @pc, @ir, @return = c + Instruction[:call, :main],
     Stack.new, c.length - 1, nil, []
    resolve_references
  end

  ##
  # Resolve all jump and call instructions to actual addresses instead of labels.

  def resolve_references
    label_addresses = @code.each_with_index.reduce({}) do |label_map, (bytecode, index)|
      if :label === bytecode.instruction
        label_map[bytecode.arguments[0]] = index
      end
      label_map
    end
    @code.map! do |bytecode|
      case bytecode.instruction
      when :call, :jump, :jumpi, :jumpz, :jumpnz
        bytecode.arguments = [label_addresses[bytecode.arguments[0]]]
        bytecode
      else
        bytecode
      end
    end
  end

  ##
  # Retrieve next instruction and execute it.
  
  def step
    @ir = @code[@pc += 1]
    execute
  end

  ##
  # Instruction dispatcher.
  
  def execute
    # Debugging output.
    #puts "Return: #{@return.map(&:to_s).join(', ')}."
    #puts "Instruction: #@pc, #@ir."
    #puts "Stack: #{@stack.to_s}."
    #puts "-----------------"
    #########
    case (sym = (@ir || Instruction.new(:noop, [])).instruction)
    when :label
    when :noop
    when :pushstack
      pop_count, accumulator = @ir.arguments[0], []
      pop_count.times { accumulator.push(@stack.pop) }
      new_stack = @stack.increment
      new_stack.push(*accumulator.reverse)
      @stack = new_stack
    when :return
      parent = @stack.parent
      @ir.arguments[0].times { parent.push(@stack.store.shift) }
      @stack = parent
      @pc = @return.pop
    when :call
      @return.push(@pc)
      @pc = @ir.arguments[0]
    when :initvar
      len = @ir.arguments[0]
      @stack.push(*[0] * len)
    when :pop
      result = nil
      @ir.arguments[0].times { result = @stack.pop }
      result
    when :loadc
      @stack.push(@ir.arguments[0])
    when :load
      starting, count = @stack.pop, @ir.arguments[0]
      (0...count).each do |i|
        @stack.push @stack[starting + i]
      end
    when :store
      ending = @stack.pop + (count = @ir.arguments[0]) - 1
      address = @stack.sp
      (0...count).each do |i|
        @stack[ending - i] = @stack[address - i]
      end
    when :loada
      starting, count = *@ir.arguments
      (0...count).each do |i|
        @stack.push @stack[starting + i]
      end
    when :storea
      ending = @ir.arguments[0] + (count = @ir.arguments[1]) - 1
      top_address = @stack.sp
      (0...count).each do |i|
        value = @stack[top_address - i]
        @stack[ending - i] = value
      end
    when :jump
      @pc = @ir.arguments[0]
    when :jumpz
      @pc = @ir.arguments[0] if @stack.pop == 0
    when :jumpnz
      @pc = @ir.arguments[0] if @stack.pop != 0
    when :jumpi
      @pc = @ir.arguments[0] + @stack.pop
    when :dup
      @stack.push @stack.top_value
    when :*, :/, :+, :-, :%
      right = @stack.pop
      left = @stack.pop
      @stack.push left.send(sym, right)
    when :==, :!=, :<, :<=, :>, :>=
      right = @stack.pop
      left = @stack.pop
      @stack.push(left.send(sym, right) ? 1 : 0)
    when :&, :|
      right = stack.pop
      left = stack.pop
      @stack.push(left.send(sym, right))
    when :-@
      @stack.push @stack.pop.send(sym)
    when :!
      @stack.push(@stack.pop != 0 ? 0 : 1)
    else
      raise StandardError, "Unrecognized operation: #{sym}."
    end
  end

  ##
  # Execute all the instructions in sequence.

  def run
    step; step while @ir
  end

end
