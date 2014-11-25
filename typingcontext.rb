class TypingContext

  attr_writer :latest_declaration

  class EmptyDeclaration
    def self.offset
      0
    end
    def self.size
      0
    end
  end

  def initialize(parent = {}, level = 0, latest_declaration = EmptyDeclaration)
    @context, @parent, @current_function, @level = {}, parent, nil, level
    @latest_declaration = latest_declaration
  end

  def latest_declaration
    @latest_declaration || @parent.latest_declaration
  end

  def []=(key, value)
    @context[key] = value
  end

  def increment
    self.class.new(self, @level + 1, @latest_declaration)
  end

  def [](key)
    @context[key] || @parent[key]
  end

  ##
  # When we are type checking a function declaration we need to set the current function so that
  # we can verify all return statements conform to the return type of the function.

  def current_function=(func)
    @current_function = func
  end

  def current_function
    @current_function || @parent.current_function
  end

end
