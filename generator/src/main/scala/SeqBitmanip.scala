package torture

import scala.collection.mutable.ArrayBuffer
import Rand._

class SeqBitmanip(xregs: HWRegPool, use_zba: Boolean, use_zbb: Boolean, use_64bit_opcodes: Boolean) extends InstSeq //TODO: better configuration
{
  override val seqname = "bitmanip"

  def seq_unary(op: Opcode) = () =>
  {
    val src = reg_read_any(xregs)
    val dest = reg_write(xregs, src)
    insts += op(dest, src)
  }

  def seq_src1(op: Opcode) = () =>
  {
    val src1 = reg_read_any(xregs)
    val dest = reg_write(xregs, src1)
    insts += op(dest, src1, src1)
  }

  def seq_src1_immfn(op: Opcode, immfn: () => Int) = () =>
  {
    val src1 = reg_read_any(xregs)
    val dest = reg_write(xregs, src1)
    val imm = Imm(immfn())
    insts += op(dest, src1, imm)
  }

  def seq_src1_zero(op: Opcode) = () =>
  {
    val src1 = reg_read_any(xregs)
    val dest = reg_write(xregs, src1)
    val tmp = reg_write_visible(xregs)
    insts += ADDI(tmp, reg_read_zero(xregs), Imm(rand_imm()))
    insts += op(dest, tmp, tmp)
  }

  def seq_src2(op: Opcode) = () =>
  {
    val src1 = reg_read_any(xregs)
    val src2 = reg_read_any(xregs)
    val dest = reg_write(xregs, src1, src2)
    insts += op(dest, src1, src2)
  }

  def seq_src2_zero(op: Opcode) = () =>
  {
    val src1 = reg_read_any(xregs)
    val dest = reg_write(xregs, src1)
    val tmp1 = reg_write_visible(xregs)
    val tmp2 = reg_write_visible(xregs)
    insts += ADDI(tmp1, reg_read_zero(xregs), Imm(rand_imm()))
    insts += ADDI(tmp2, reg_read_zero(xregs), Imm(rand_imm()))
    insts += op(dest, tmp1, tmp2)
  }

  val candidates = new ArrayBuffer[() => insts.type]
  val oplist = new ArrayBuffer[Opcode]
  val unary_oplist = new ArrayBuffer[Opcode]

  if (use_zba)
  {
    if (use_64bit_opcodes)
    {
      candidates += seq_src1_immfn(SLLI_UW, rand_shamt)
      oplist += (ADD_UW, SH1ADD_UW, SH2ADD_UW, SH3ADD_UW)
    }

    oplist += (SH1ADD, SH2ADD, SH3ADD)
  }

  if (use_zbb)
  {
    if (use_64bit_opcodes)
    {
      candidates += seq_src1_immfn(RORIW, rand_shamtw)
      unary_oplist += (CLZW, CTZW)
      unary_oplist += (CPOPW)
      oplist += (ROLW, RORW)
    }

    candidates += seq_src1_immfn(RORI, if (use_64bit_opcodes) rand_shamt else rand_shamtw)
    oplist += (ANDN, ORN, XNOR)
    unary_oplist += (CLZ, CTZ, CPOP)
    oplist += (MAX, MAXU, MIN, MINU)
    unary_oplist += (SEXT_B, SEXT_H, ZEXT_H)
    oplist += (ROL, ROR)
    unary_oplist += (ORC_B)
    unary_oplist += (REV8)
  }

  for (op <- oplist)
  {
    candidates += seq_src1(op)
    candidates += seq_src1_zero(op)
    candidates += seq_src2(op)
    candidates += seq_src2_zero(op)
  }

  for (op <- unary_oplist)
  {
    candidates += seq_unary(op)
  }

  rand_pick(candidates)()
}
