import chisel3._
import chisel3.util._

class Accelerator extends Module {
  val io = IO(new Bundle {
    val start = Input(Bool())
    val done = Output(Bool())

    val address = Output(UInt (16.W))
    val dataRead = Input(UInt (32.W))
    val writeEnable = Output(Bool ())
    val dataWrite = Output(UInt (32.W))
  })

  //Write here your code
  val idle :: xloop :: yloop :: check :: write0 :: write255 :: negx :: posx :: negy:: posy :: done :: Nil = Enum (11)
  val stateReg = RegInit(idle)

  //Support registers
  val inReg = RegInit(0.U(16.W))
  val xReg = RegInit(0.U(32.W))
  val yReg = RegInit(0.U(32.W))

  //Default values
  io.writeEnable := false.B
  io.address := 0.U(16.W)
  io.dataWrite := 0.U(32.W)
  io.done := false.B

  //FSMD switch
  switch(stateReg) {

    is(idle) {
      when(io.start) {
        stateReg := xloop
      }
    }

    is(xloop) {
      when(xReg < 20.U(32.W)) {
        yReg := 0.U(32.W)
        stateReg := yloop
      }.otherwise {
        stateReg := done
      }
    }

    is(yloop) {
      when(yReg < 20.U(32.W)) {
        inReg := xReg + yReg * 20.U(16.W)
        io.writeEnable := false.B
        stateReg := check
      }.otherwise {
        xReg := xReg + 1.U(32.W)
        stateReg := xloop
      }
    }

    is(check) {
      io.address := inReg
      when(yReg === 0.U(32.W) || yReg === 19.U(32.W) || xReg === 0.U(32.W) || xReg === 19.U(32.W)
            || io.dataRead === 0.U(32.W)) {
        stateReg := write0
      }.otherwise {
        stateReg := negx
      }
    }

    is(write0) {
      io.address := (inReg + 400.U(16.W))
      io.dataWrite := 0.U(32.W)
      io.writeEnable := true.B
      yReg := yReg + 1.U(32.W)
      stateReg := yloop
    }

    is(negx) {
      io.address := (inReg - 1.U(16.W))
      when(io.dataRead === 0.U(32.W)) {
        stateReg := write0
      }.otherwise {
        stateReg := posx
      }
    }

    is(posx) {
      io.address := (inReg + 1.U(16.W))
      when(io.dataRead === 0.U(32.W)) {
        stateReg := write0
      }.otherwise {
        stateReg := negy
      }
    }

    is(negy) {
      io.address := (inReg - 20.U(16.W))
      when(io.dataRead === 0.U(32.W)) {
        stateReg := write0
      }.otherwise {
        stateReg := posy
      }
    }

    is(posy) {
      io.address := (inReg + 20.U(16.W))
      when(io.dataRead === 0.U(32.W)) {
        stateReg := write0
      }.otherwise {
        stateReg := write255
      }
    }

    is(write255) {
      io.address := (inReg + 400.U(16.W))
      io.dataWrite := 255.U(32.W)
      io.writeEnable := true.B
      yReg := yReg + 1.U(32.W)
      stateReg := yloop
    }

    is(done) {
      io.done := true.B
      stateReg := done
    }
  }

}
