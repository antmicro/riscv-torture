package torture


import scala.collection.mutable.ArrayBuffer
import Rand._

class SeqCompressed(xregs: HWRegPool, mem: Mem, use_zcb: Boolean, use_zcmp: Boolean, use_zcmt: Boolean, use_64bit_opcodes: Boolean) extends InstSeq
{
  override val seqname = "compressed"
  val crashes = ArrayBuffer[Label](Label("crash_backward"), Label("crash_forward"))
  val candidates = new ArrayBuffer[() => insts.type]

  def seq_srcdst(op: Opcode) = () =>
  {
    val srcdst = reg_write_visible_popular(xregs)
    insts += op(srcdst)
  }

  def seq_srcdst_src2(op: Opcode) = () =>
  {
    val srcdst = reg_write_visible_popular(xregs)
    val src2 = reg_read_popular(xregs)
    insts += op(srcdst, src2)
  }

  def seq_load_addrfn(op: Opcode, addrfn: (Int) => Int, imm: Int) = () =>
  {
    val reg_addr = reg_write_hidden_popular(xregs)
    val reg_dest = reg_write_visible_popular(xregs)
    val addr = addrfn(mem.size)

    insts += LA(reg_addr, BaseImm(mem.toString, addr-imm))
    insts += op(reg_dest, RegImm(reg_addr, imm))
  }

  def seq_store_addrfn(op: Opcode, addrfn: (Int) => Int, imm: Int) = () =>
  {
    val reg_addr = reg_write_hidden_popular(xregs)
    val reg_src = reg_read_visible_popular(xregs)
    val addr = addrfn(mem.size)
    insts += LA(reg_addr, BaseImm(mem.toString, addr-imm))
    insts += op(reg_src, RegImm(reg_addr, imm))
  }

  if (use_zcb)
  {
    candidates += seq_load_addrfn(C_LBU, rand_addr_b, rand_range(0, 3))
    candidates += seq_load_addrfn(C_LH, rand_addr_h, rand_range(0, 1) * 2)
    candidates += seq_load_addrfn(C_LHU, rand_addr_h, rand_range(0, 1) * 2)
  
    candidates += seq_store_addrfn(C_SB, rand_addr_b, rand_range(0,3))
    candidates += seq_store_addrfn(C_SH, rand_addr_h, rand_range(0,1) * 2)

    candidates += seq_srcdst_src2(C_MUL)

    val srcdst_oplist = new ArrayBuffer[Opcode]
    srcdst_oplist += (C_NOT, C_ZEXT_B, C_SEXT_B, C_ZEXT_H, C_SEXT_H)
    if (use_64bit_opcodes)
    {
      srcdst_oplist += C_ZEXT_W 
    }
    for (op <- srcdst_oplist)
    {
      candidates += seq_srcdst(op)
    }
  }

  for (op <- srcdst_oplist)
  {
    candidates += seq_srcdst(op)
  }
  
  rand_pick(candidates)()
}
