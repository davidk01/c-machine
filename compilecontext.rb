module CMachineGrammar

  ##
  # Keeps track of some global counters and data as we compile C-variant code into
  # VM code.

  class CompileContext

    def initialize(outer_context = nil)
      @label_counter = -1
      @outer_context = outer_context
    end

    ##
    # When getting a new label we want to go all the way to the root context because
    # we want generated labels to be unique.

    def get_label
      if @outer_context
        return @outer_context.get_label
      end
      label = "label#{@label_counter += 1}".to_sym
    end

    ##
    # Incrementing means we are entering a new block which means we have to be careful with
    # how we do variable lookup and declaration when it comes to assigning the variables stack
    # addresses.

    def increment
      self.class.new(self)
    end

  end

end
