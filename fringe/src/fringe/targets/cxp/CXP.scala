package fringe.targets.cxp

import chisel3._
import fringe.{AbstractAccelTop, BigIP, TopInterface}
import fringe.targets.DeviceTarget

class CXP extends DeviceTarget {

  override val magPipelineDepth: Int = 0
  override def regFileAddrWidth(n: Int): Int = 32

  override def topInterface(reset: Reset, accel: AbstractAccelTop): TopInterface = {
    val io = IO(new CXPInterface)

    // CXP Fringe
    val blockingDRAMIssue = false // Allow only one in-flight request, block until response comes back
    val fringe = Module(new FringeCXP(blockingDRAMIssue, io.axiLiteParams, io.axiParams))

    // Fringe <-> Host connections
    fringe.io.S_AXI <> io.S_AXI

    // Fringe <-> DRAM connections
    io.M_AXI <> fringe.io.M_AXI

    // io.TOP_AXI <> fringe.io.TOP_AXI
    // io.DWIDTH_AXI <> fringe.io.DWIDTH_AXI
    // io.PROTOCOL_AXI <> fringe.io.PROTOCOL_AXI
    // io.CLOCKCONVERT_AXI <> fringe.io.CLOCKCONVERT_AXI

    // io.rdata handled by bridge inside FringeCXP
    io.rdata := DontCare

    accel.io.argIns := fringe.io.argIns
    fringe.io.argOuts.zip(accel.io.argOuts) foreach { case (fringeArgOut, accelArgOut) =>
      fringeArgOut.bits := accelArgOut.port.bits
      fringeArgOut.valid := accelArgOut.port.valid
    }
    fringe.io.argEchos.zip(accel.io.argOuts) foreach { case (fringeArgOut, accelArgOut) =>
      accelArgOut.echo := fringeArgOut
    }

    // accel.io.argIOIns := fringe.io.argIOIns
    // fringe.io.argIOOuts.zip(accel.io.argIOOuts) foreach { case (fringeArgOut, accelArgOut) =>
    //     fringeArgOut.bits := accelArgOut.bits
    //     fringeArgOut.valid := 1.U
    // }
    fringe.io.externalEnable := false.B
    fringe.io.memStreams <> accel.io.memStreams
    fringe.io.heap <> accel.io.heap
    accel.io.enable := fringe.io.enable
    fringe.io.done := accel.io.done
    fringe.reset := !reset.toBool
    accel.reset := fringe.io.reset
    // accel.reset := ~reset.toBool
    // io.is_enabled := ~accel.io.enable

    io
  }

  def makeBigIP: BigIP = new fringe.targets.cxp.BigIPCXP

}