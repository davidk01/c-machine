module CMachineGrammar

  ##
  # Keeps track of some global counters and data as we compile C-variant code into
  # VM code.

  class CompileContext

    def initialize(outer_context = nil, level = 0)
      @label_counter, @structs, @functions = -1, {}, {}
      @outer_context, @variables, @level = outer_context, [], level
    end

    ##
    # Struct declarations are going to be global for the time being so we recurse to the root
    # context.

    def assign_struct_data(name, data)
      if @outer_context
        return @outer_context.assign_struct_data(name, data)
      end
      @structs[name] = data
    end

    ##
    # We assign only to the global context so we have to retrieve from the global context as
    # well.

    def get_struct_data(name)
      if @outer_context
        return @outer_context.get_struct_data(name)
      end
      @structs[name]
    end

    ##
    # This one is pretty simple. Start from the back and go until we find a variable that
    # matches in the current context. If we can't find it in current context then keep looking
    # in outer context.

    def get_variable_data(name)
      i = 0
      while (data = @variables[i -= 1])
        if data.name == name
          return data
        end
      end
      if @outer_context
        return @outer_context.get_variable_data(name)
      end
      raise StandardError, "Could not find a variable by that name: #{name}."
    end

    ##
    # Adding a variable can be a little tricky because we need to figure out offsets which
    # depends on sizes of already declared variables but this is handled in the compile method
    # so we don't have to worry about it here.

    def add_variable(data)
      @variables.push(data)
    end

    ##
    # See if there is anything in the current context. If not then see if there is an outer context
    # and return that. Otherwise return nil.

    def latest_declaration
      if (v = @variables[-1])
        return v
      end
      if @outer_context
        return @outer_context.latest_declaration
      end
      nil
    end

    ##
    # When getting a new label we want to go all the way to the root context because
    # we want generated labels to be unique.

    def get_label
      if @outer_context
        return @outer_context.get_label
      end
      "label#{@label_counter += 1}".to_sym
    end

    ##
    # Incrementing means we are entering a new block which means we have to be careful with
    # how we do variable lookup and declaration when it comes to assigning the variables stack
    # addresses.

    def increment
      self.class.new(self, @level + 1)
    end

  end

end
