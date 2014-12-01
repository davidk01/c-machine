# We need to keep track of the top of the stack so
# encapsulate that logic in one place to make sure the invariant is enforced.
class Stack < Struct.new(:store, :sp, :parent, :function_variables, :local_variables)

  def initialize
    super([], -1, [], [], [])
  end

  def length
    store.length
  end

  def function_var_info(name, offset, size)
    function_variables << [offset, size, name]
  end

  def local_var_info(name, offset, size)
    local_variables << [offset, size, name]
    local_variables.uniq!
  end

  def to_s
    func_variables = function_variables.map do |(offset, size, name)|
      vars = store[offset...(offset + size)]
      "[#{name} : #{vars.map(&:to_s).join(', ')}]"
    end
    local_vars = local_variables.map do |(offset, size, name)|
      "[#{name} : #{store[offset...(offset + size)].map(&:to_s).join(', ')}]"
    end
    rest_offsets = local_variables[-1] || function_variables[-1] || [0, 0, nil]
    rest = store[(rest_offsets[0] + rest_offsets[1])..-1].map(&:to_s)
    current = (func_variables + local_vars + rest).join(', ')
    parent ? parent.to_s + "\n ---\n " + current : current
  end

  def pop
    self.sp = self.sp - 1
    self.store.pop
  end

  def push(*args)
    self.sp = self.sp + args.length
    self.store.push(*args);
  end

  def [](val)
    self.store[val]
  end

  def []=(address, val)
    self.store[address] = val
  end

  def top_value
    self.store[-1]
  end

  def increment
    (new_scope = Stack.new).parent = self
    new_scope
  end

  def decrement
    parent
  end

end
