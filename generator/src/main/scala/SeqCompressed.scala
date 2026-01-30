package torture


import scala.collection.mutable.ArrayBuffer
import Rand._

class SeqCompressed(xregs: HWRegPool, mem: Mem, use_zcb: Boolean, use_zcmp: Boolean, use_zcmt: Boolean, use_64bit_opcodes: Boolean) extends InstSeq
{
  override val seqname = "compressed"
  val crashes = ArrayBuffer[Label](Label("crash_backward"), Label("crash_forward"))
  val candidates = new ArrayBuffer[() => insts.type]

  val legalZcmpRegSet = ArrayBuffer(
    ArrayBuffer("x1"),                                                                                   // {ra}
    ArrayBuffer("x1", "x8"),                                                                             // {ra,s0}
    ArrayBuffer("x1", "x8", "x9"),                                                                       // {ra,s0-s1}
    ArrayBuffer("x1", "x8", "x9", "x18"),                                                                // {ra,s0-s2}
    ArrayBuffer("x1", "x8", "x9", "x18", "x19"),                                                         // {ra,s0-s3}
    ArrayBuffer("x1", "x8", "x9", "x18", "x19", "x20"),                                                  // {ra,s0-s4}
    ArrayBuffer("x1", "x8", "x9", "x18", "x19", "x20", "x21"),                                           // {ra,s0-s5}
    ArrayBuffer("x1", "x8", "x9", "x18", "x19", "x20", "x21", "x22"),                                    // {ra,s0-s6}
    ArrayBuffer("x1", "x8", "x9", "x18", "x19", "x20", "x21", "x22", "x23"),                             // {ra,s0-s7}
    ArrayBuffer("x1", "x8", "x9", "x18", "x19", "x20", "x21", "x22", "x23", "x24"),                      // {ra,s0-s8}
    ArrayBuffer("x1", "x8", "x9", "x18", "x19", "x20", "x21", "x22", "x23", "x24", "x25"),               // {ra,s0-s9}
    ArrayBuffer("x1", "x8", "x9", "x18", "x19", "x20", "x21", "x22", "x23", "x24", "x25", "x26", "x27")  // {ra,s0-s11}
  )

  def generate_jump_table(idx: Integer, next_insn_label: Label): ProgSeg =
  {
    val table = new ProgSeg("jump_table_" + SeqCompressed.jump_table_idx.toString, Some(64))
    SeqCompressed.jump_table_idx += 1

    for (i <- 0 to 255)
    {
      val label : Label = if (i == idx) next_insn_label else rand_pick(crashes)
      table.insts += (if (use_64bit_opcodes) DWORD(label) else WORD(label))
    }

    extra_code += ProgSegDump(table)
    table
  }

  def rand_stack_adj(set: ArrayBuffer[String]): Integer = {
    val rlist = set.length match {
      case 13 => 15
      case x if x > 0 && x < 13  => set.length + 3
    }
    if (!use_64bit_opcodes)
    {
      val opts = rlist match {
        case x if x >= 4 && x <= 7 => ArrayBuffer(16, 32, 48, 64)
        case x if x >= 8 && x <= 11 => ArrayBuffer(32, 48, 64, 80)
        case x if x >= 12 && x <= 14 => ArrayBuffer(48, 64, 80, 96)
        case 15 => ArrayBuffer(64, 80, 96, 112)
        case _ => throw new IllegalArgumentException(s"Bad rlist=$rlist for RV32I")
      }
      rand_pick(opts)
    }
    else
    {
      val opts = rlist match {
        case x if x >= 4 && rlist <= 5 => ArrayBuffer(16, 32, 48, 64)
        case x if x >= 6 && rlist <= 7 => ArrayBuffer(32, 48, 64, 80)
        case x if x >= 8 && rlist <= 9 => ArrayBuffer(48, 64, 80, 96)
        case x if x >= 10 && rlist <= 11 => ArrayBuffer(64, 80, 96, 112)
        case x if x >= 12 && rlist <= 13 => ArrayBuffer(80, 96, 112, 128)
        case 14 => ArrayBuffer(96, 112, 128, 144)
        case 15 => ArrayBuffer(112, 128, 144, 160)
        case _ => throw new IllegalArgumentException(s"Bad rlist=$rlist for RV64")
      }
      rand_pick(opts)
    }
  }

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

  def seq_store_sreg_sreg(op: Opcode) = () =>
  {
    val reg_dest = reg_write_visible_sreg(xregs)
    val reg_dest2 = reg_write_visible_other_sreg(xregs, reg_dest)
    insts += op(AbiReg(reg_dest), AbiReg(reg_dest2))
  }

  def seq_load_sreg_sreg(op: Opcode) = () =>
  {
    val reg_src = reg_read_visible_sreg(xregs)
    val reg_src2 = reg_read_visible_sreg(xregs)
    insts += op(AbiReg(reg_src), AbiReg(reg_src2))
  }

  def seq_jt(op: Opcode, idx: Integer, check_ra: Boolean) = () =>
  {
    val next_insn_label = Label("jt_target" + SeqCompressed.next_insn_label_ctr.toString)
    SeqCompressed.next_insn_label_ctr += 1
    val table = generate_jump_table(idx, next_insn_label)

    val reg_src = reg_write_hidden(xregs)
    insts += LA(reg_src, table.label)
    insts += CSRW(CSR("jvt"), reg_src)
    insts += op(Imm(idx))
    insts += next_insn_label.inst
    
    if (check_ra)
    {
      val reg_ra = reg_read_name(xregs, "x1")
      val reg_src2 = reg_write_other(xregs, reg_ra)
      insts += LA(reg_src2, next_insn_label)
      insts += BNE(reg_src2, reg_ra, rand_pick(crashes))
    }
    insts.asInstanceOf[insts.type]
  }

  // TODO: RV32E allows only for the first three sets
  def seq_pushpop(op: Opcode, stack_adj_sign: Int) = () =>
  {
    val selectedSet: ArrayBuffer[String] = rand_pick(legalZcmpRegSet)
    var stack_adj = rand_stack_adj(selectedSet)
    val opList = new ArrayBuffer[Operand]

    val reg_ra = reg_write_name(xregs, selectedSet(0))
    opList += reg_ra;

    if (selectedSet.length > 1)
    {
      val reg_first_range = reg_write_name(xregs, selectedSet(1))

      if (selectedSet.length > 2)
      {
        val reg_sec_range = reg_write_name(xregs, selectedSet(2))
        opList += new RegRange(reg_first_range, reg_sec_range)
        if (selectedSet.length > 3)
        {
          val reg_third_range = reg_write_name(xregs, selectedSet(3))
          if (selectedSet.length > 4)
          {
            val reg_last_range = reg_write_name(xregs, selectedSet.last)
            opList += new RegRange(reg_third_range, reg_last_range)
          }
          else
          {
            opList += reg_third_range
          }
        }
      }
      else
      {
        opList += reg_first_range
      }
    }

    val next_insn_label = Label("popret_target" + SeqCompressed.next_insn_label_ctr.toString)
    SeqCompressed.next_insn_label_ctr += 1

    // SP needs to be valid
    val reg_sp = reg_write_name(xregs, "x2")
    insts += LA(reg_sp, Label("stack_top"))

    insts += op(new OpList(opList:_*), Imm(stack_adj_sign * stack_adj))

    insts.asInstanceOf[insts.type]
  }

  def seq_popret(op: Opcode, check_a0_zero: Boolean) = () =>
  {
    val next_insn_label = Label("popret_target" + SeqCompressed.next_insn_label_ctr.toString)
    SeqCompressed.next_insn_label_ctr += 1

    val reg_ra = reg_read_name(xregs, "x1")
    insts += LA(reg_ra, next_insn_label)
    
    seq_pushpop(CM_PUSH, -1)()
    var opList = insts.last.operands(0)
    var stack_adj = -insts.last.operands(1).asInstanceOf[Imm].toString.toInt
    insts += op(opList, Imm(stack_adj))
    insts += next_insn_label.inst
    
    if (check_a0_zero)
    {
      val reg_a0 = reg_read_name(xregs, "x10")
      val reg_zero = reg_read_zero(xregs)
      insts += BNE(reg_a0, reg_zero, rand_pick(crashes))
    }
    insts.asInstanceOf[insts.type]
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

  if (use_zcmp)
  {
    candidates += seq_store_sreg_sreg(CM_MVSA01)
    candidates += seq_load_sreg_sreg(CM_MVA01S)

    candidates += seq_popret(CM_POPRET, false)
    candidates += seq_popret(CM_POPRETZ, true)
    
    candidates += seq_pushpop(CM_PUSH, -1)
    candidates += seq_pushpop(CM_POP, 1)
  }

  if (use_zcmt)
  {
    candidates += seq_jt(CM_JT, rand_range(0, 31), false)
    candidates += seq_jt(CM_JALT, rand_range(32, 255), true)
  }
  
  rand_pick(candidates)()
}

object SeqCompressed
{
  var next_insn_label_ctr = 0
  var jump_table_idx = 0

  def generate_stack_space(): ProgSeg =
  {
    val stack = new ProgSeg("stack", Some(16))
    stack.insts += SPACE(Imm(4096))
    stack.insts += Label("stack_top").inst

    stack
  }
}
