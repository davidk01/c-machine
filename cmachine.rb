require_relative './cmachinestack'
require_relative './cmachineheap'

class CMachine

  ##
  # Need a convenient way to see all the instructions. Simple comment for the time being
  # :label l (symbolic labels for jump instructions)
  # :pop (decrement the stack pointer)
  # :pushstack k (add another stack on top of the current one for a function call and move k values
  #               from the current stack to the new one)
  # :arginfo symbol offset len (argument information for debugging purposes)
  # :call label (call a function by jumping to the given label/address after saving @pc)
  # :return (jump back to a saved @pc)
  # :loadc c (push a constant on top of the stack)
  # :load c (take the top of the stack as a starting address and load c values from it)
  # :store c (take the top of the stack as a starting address and store c values to it)
  # :loada s c (load c elements starting at s)
  # :storea s c (store c elements starting at s)
  # :initvar s l (initialize l zeroes starting at s)
  # :malloc (allocate space on the heap and return a pointer to it. amount of allocation is 
  #          top of stack)
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
    def to_s; "#{instruction}#{arguments}"; end
  end

  ##
  # Because of my decision to separate heap and stack into distinct regions without
  # using any registers I need to tag heap addresses. This class acts as that tag.

  class HeapAddress < Struct.new(:address)

    def to_s
      "h#{address}"
    end

    def +(other)
      self.class.new(address + other)
    end

  end

  ##
  # Readers for most of the internal state.

  attr_reader :code, :stack, :pc, :ir, :return, :heap

  def initialize(c)
    @code, @stack, @pc, @ir, @return, @heap = c + Instruction[:halt] + Instruction[:call, :main],
      Stack.new, c.length, nil, [], Heap.new
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

  def step
    @ir = @code[@pc += 1]; execute; nil
  end

  def debug
    # Debugging output.
    puts "Return: #{@return.map(&:to_s).join(', ')}."
    puts "stack pointer: #{@stack.length}."
    puts "Function arguments: #{@fp}."
    puts "Stack: #{@stack.to_s}."
    puts "Heap: #{@heap}."
    puts "Instruction: #@pc, #@ir."
    puts "-----------------"
    #########
  end

  def execute
    debug
    case (sym = (@ir || Instruction.new(:noop, [])).instruction)
    when :halt
      raise StandardError, "Halting."
    when :label
    when :noop
    when :arginfo
      name, offset, size = *@ir.arguments
      @stack.function_var_info(name, offset, size)
    when :break
      require 'pry'; binding.pry
    when :pushstack
      pop_count, accumulator = @ir.arguments[0], []
      pop_count.times { accumulator.push(@stack.pop) }
      new_stack = @stack.increment
      new_stack.push(*accumulator.reverse)
      @stack = new_stack
      @fp = new_stack.store.length
    when :return
      parent = @stack.parent
      @ir.arguments[0].times { parent.push(@stack.store.shift) }
      @stack = parent
      @pc = @return.pop
    when :call
      @return.push(@pc)
      @pc = @ir.arguments[0]
    when :initvar
      offset, len, name = @ir.arguments[0], @ir.arguments[1], @ir.arguments[2]
      if offset > @stack.sp
        len.times { @stack.push(0) }
      elsif offset <= @stack.sp
        (0...len).each {|i| @stack.store[offset + i] = 0}
      else
        raise StandardError, "This should not happen: sp = #{@stack.sp}, offset = #{offset}."
      end
      @stack.local_var_info(name, offset, len)
    when :malloc
      starting_address = HeapAddress.new(@heap.allocate(@stack.pop))
      @stack.push(starting_address)
    when :pop
      result = nil
      @ir.arguments[0].times { result = @stack.pop }
      result
    when :loadc
      @stack.push(@ir.arguments[0])
    when :load
      starting, count = @stack.pop, @ir.arguments[0]
      if HeapAddress === starting
        (0...count).each {|i| @stack.push @heap.heap[starting.address + i]}
      else
        (0...count).each {|i| @stack.push @stack[starting + i]}
      end
    when :store
      # TODO: figure out a cleaner way to do this
      count = @ir.arguments[0]
      address = @stack.pop
      if HeapAddress === address
        ending = address.address + count - 1
        (0...count).each {|i| @heap.heap[ending - i] = @stack[@stack.sp - i]}
      else
        ending = address + count - 1
        (0...count).each {|i| @stack[ending - i] = @stack[@stack.sp - i]}
      end
    when :loada
      starting, count = *@ir.arguments
      (0...count).each {|i| @stack.push @stack[starting + i]}
    when :storea
      count = @ir.arguments[1]
      ending = @ir.arguments[0] + count - 1
      sp = @stack.sp
      (0...count).each {|i| @stack[ending - i] = @stack[sp - i]}
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
  # Execute instructions until halt.

  def run
    step; step while @ir
  end

end
