import spatial.dsl._

@spatial object InnerProduct extends SpatialApp {
  def main(args: Array[String]): Void = {
    type T = FixPt[TRUE,_24,_8]
    
    // *** Set tile size as compile-time constant
    val tileSize = 64
    
    // *** Allow dynamically sized array
    val len = ArgIn[Int]
    setArg(len,args(0).to[Int])
    
    // *** Use ArgIn to generate data and configure DRAM
    val vec1 = Array.tabulate[T](len){i => i.to[T]}
    val vec2 = Array.tabulate[T](len){i => (len - i).to[T]}
    val d1 = DRAM[T](len)
    val d2 = DRAM[T](len)
    setMem(d1, vec1)
    setMem(d2, vec2)

    val x = ArgOut[T]
    Accel {
      // *** Create local SRAMs with fixed tile size
      val s1 = SRAM[T](tileSize)
      val s2 = SRAM[T](tileSize)
      
      // *** Loop over each tile
      x := Reduce(Reg[T](0))(len by tileSize){tile => 
        s1 load d1(tile::tile+tileSize)
        s2 load d2(tile::tile+tileSize)
        
        // *** Return local accumulator to map function of outer Reduce
        Reduce(Reg[T](0))(tileSize by 1){i => 
          s1(i) * s2(i)
        }{_+_}
      }{_+_}
    }
    
    val gold = vec1.zip(vec2){_*_}.reduce{_+_}
    
    assert(gold == getArg(x), r"Expected ${gold}, got ${getArg(x)}!")
  }
}
