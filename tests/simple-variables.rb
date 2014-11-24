require 'minitest/autorun'
require_relative '../cmachinegrammar'

class TestSimpleVariables < MiniTest::Unit::TestCase

  def setup
    @code = File.read(File.expand_path(File.dirname(__FILE__) + '/../examples/variables'))
    @context = CMachineGrammar::CompileData.new
    @ast = CMachineGrammar.parse(@code)
    @ast.compile(@context)
  end

  def test_basic_struct_size
    positions = [0, 1, 2, 5, 8]
    @context.variables.zip(positions).each do |var, pos|
      assert_equal(var[0], pos, "Incorrect variable position: #{var[1]}.")
    end
  end

end
