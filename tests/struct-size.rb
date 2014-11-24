require 'minitest/autorun'
require_relative '../cmachinegrammar'

class TestStructSize < MiniTest::Unit::TestCase

  def setup
    @code = File.read(File.expand_path(File.dirname(__FILE__) + '/../examples/struct'))
    @context = CMachineGrammar::CompileData.new
    @ast = CMachineGrammar.parse(@code)
    @ast.compile(@context)
  end

  def test_basic_struct_size
    struct_size = @context.structs['BasicStruct'].size(@context)
    assert_equal(3, struct_size, 'Struct with 3 basic members is of size 3.')
  end

  def test_another_basic_struct_size
    struct_size = @context.structs['AnotherBasicStruct'].size(@context)
    assert_equal(3, struct_size, 'Struct with 3 basic members if of size 3.')
  end

  def test_non_basic_struct_size
    struct_size = @context.structs['NonBasicStruct'].size(@context)
    assert_equal(14, struct_size, 'Struct with more complicated members should have correct size.')
  end

  def test_basic_struct_member_offsets
    struct = @context.structs['BasicStruct']
    assert_equal(0, struct.offset(@context, 'a'), 'First member is at offset 0.')
    assert_equal(1, struct.offset(@context, 'b'), 'Second member is at offset 1.')
    assert_equal(2, struct.offset(@context, 'c'), 'Third member is at offset 2.')
  end

  def test_another_basic_struct_member_offsets
    struct = @context.structs['AnotherBasicStruct']
    assert_equal(0, struct.offset(@context, 'a'), 'First member is at offset 0.')
    assert_equal(1, struct.offset(@context, 'b'), 'Second member is at offset 1.')
    assert_equal(2, struct.offset(@context, 'c'), 'Third member is at offset 2.')
  end

  def test_non_basic_struct_member_offsets
    struct = @context.structs['NonBasicStruct']
    assert_equal(0, struct.offset(@context, 'a'), 'First member is at offset 0.')
    assert_equal(1, struct.offset(@context, 'b'), 'Second member is at offset 1.')
    assert_equal(11, struct.offset(@context, 'c'), 'Third member is at offset 2.')
  end

  def non_existing_member_test
    struct = @context.structs['NonBasicStruct']
    assert_raises(StandardError) { struct.offset(@context, 'd') }
  end

end
