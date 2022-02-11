package testchipip

import chisel3._
import chisel3.util._
import chisel3.experimental.{IO}
import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.devices.tilelink._
import freechips.rocketchip.regmapper._
import freechips.rocketchip.subsystem._
import freechips.rocketchip.util._
import freechips.rocketchip.tile._
import freechips.rocketchip.prci._

case class CustomBootPinParams(
  customBootAddress: BigInt = 0x80000000L, // Default is DRAM_BASE
  masterWhere: TLBusWrapperLocation = CBUS // This needs to write to clint and bootaddrreg, which are on CBUS/PBUS
)

case object CustomBootPinKey extends Field[Option[CustomBootPinParams]](Some(CustomBootPinParams()))

class WithCustomBootPinAltAddr(address: BigInt) extends Config((site, here, up) => {
  case CustomBootPinKey => up(CustomBootPinKey, site).map(p => p.copy(customBootAddress = address))
})

trait CanHavePeripheryCustomBootPin { this: BaseSubsystem =>
  val custom_boot_pin = p(CustomBootPinKey).map { params =>
    val tlbus = locateTLBusWrapper(params.masterWhere)
    val clientParams = TLMasterPortParameters.v1(
      clients = Seq(TLMasterParameters.v1(
        name = "custom-boot",
        sourceId = IdRange(0, 1),
      )),
      minLatency = 1
    )

    val inner_io = tlbus {
      val node = TLClientNode(Seq(clientParams))
      tlbus.coupleFrom(s"port_named_custom_boot_pin") ({ _ := node })

      InModuleBody {
        val custom_boot = IO(Input(Bool())).suggestName("custom_boot")
        val (tl, edge) = node.out(0)
        val inactive :: waiting_bootaddr_reg_a :: waiting_bootaddr_reg_d :: waiting_msip_a :: waiting_msip_d :: dead :: Nil = Enum(6)
        val state = RegInit(inactive)
        tl.a.valid := false.B
        tl.a.bits := DontCare
        tl.d.ready := true.B
        switch (state) {
          is (inactive) { when (custom_boot) { state := waiting_bootaddr_reg_a } }
          is (waiting_bootaddr_reg_a) {
            tl.a.valid := true.B
            tl.a.bits := edge.Put(
              toAddress = p(BootAddrRegKey).bootRegAddress.U,
              fromSource = 0.U,
              lgSize = 2.U,
              data = params.customBootAddress.U
            )._2
            when (tl.a.fire) { state := waiting_bootaddr_reg_d }
          }
          is (waiting_bootaddr_reg_d) { when (tl.d.fire) { state := waiting_msip_a } }
          is (waiting_msip_a) {
            tl.a.valid := true.B
            tl.a.bits := edge.Put(
              toAddress = (p(CLINTKey).get.baseAddress + CLINTConsts.msipOffset(0)).U, // msip for hart0
              fromSource = 0.U,
              lgSize = log2Ceil(CLINTConsts.msipBytes).U,
              data = 1.U
            )._2
            when (tl.a.fire) { state := waiting_msip_d }
          }
          is (waiting_msip_d) { when (tl.d.fire) { state := dead } }
          is (dead) { when (!custom_boot) { state := inactive } }
        }
        custom_boot
      }
    }
    val outer_io = InModuleBody {
      val custom_boot = IO(Input(Bool())).suggestName("custom_boot")
      inner_io := custom_boot
      custom_boot
    }
    outer_io
  }
}
