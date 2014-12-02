class Heap < Struct.new(:heap)

  def to_s
    "#{heap}"
  end

  def initialize
    super([])
  end

  def allocate(size)
    start = heap.length
    size.times { heap << 0 }
    start
  end

end
