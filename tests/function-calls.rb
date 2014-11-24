require 'minitest/autorun'
require_relative '../cmachinegrammar'

class TestFunctionCalls < MiniTest::Unit::TestCase

  def setup
    @code = File.read(File.expand_path(File.dirname(__FILE__) + '/../examples/functions'))
    @context = CMachineGrammar::CompileContext.new
    @ast = CMachineGrammar.parse(@code)
    @bytecode = @ast.compile(@context)
  end

  def test_valid_bytecode
    assert(!@bytecode.nil?)
  end

end

