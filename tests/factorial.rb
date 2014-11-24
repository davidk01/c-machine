require 'minitest/autorun'
require_relative '../cmachinegrammar'
require_relative '../cmachine'

class TestFunctionCalls < MiniTest::Unit::TestCase

  def setup
    @code = File.read(File.expand_path(File.dirname(__FILE__) + '/../examples/factorial'))
    @context = CMachineGrammar::CompileData.new
    @ast = CMachineGrammar.parse(@code)
    @bytecode = @ast.map {|node| node.compile(@context)}.flatten
    @machine = CMachine.new(@bytecode)
  end

  def test_valid_bytecode
    assert(!@bytecode.nil?)
    @machine.run
    assert(@machine.stack.pop == 6)
  end

end
