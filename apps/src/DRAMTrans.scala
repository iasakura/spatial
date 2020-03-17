import spatial.dsl._

@spatial object DRAMTrans extends SpatialApp {
  def main(args: Array[String]): Void = {
    // Create DRAM (malloc)
    val d = DRAM[Int](16)
    
    // Set DRAM (memcpy)
    val data = Array.tabulate[Int](16)(i => i)
    setMem(d, data)

    val x = ArgIn[Int]
    setArg(x, args(0).to[Int])
    
    Accel {
      // Create 16-element SRAM
      val s = SRAM[Int](16)
      
      // Transfer data from d to s
      s load d(0::16)
    
      // Add number to each element
      Foreach(16 by 1){i => s(i) = s(i) + x}

      // Transfer data back to d
      d(0::16) store s
    }
    
    // Print contents in memory
    printArray(getMem(d), "Result: ")

  }
}
