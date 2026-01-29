package torture

import scala.collection.mutable.ArrayBuffer

abstract class Operand

class Reg extends Operand
{
  var allocated = false
  var hwreg = new HWReg("-", false, false)

  override def toString = hwreg.toString
}

class RegNeedsAlloc(
  val hwrp: HWRegPool,
  val filter: (HWReg) => Boolean,
  val alloc: (HWReg) => Unit,
  val free: (HWReg) => Unit,
  val consec_regs: Int = 1) extends Reg
{
  val regs = new ArrayBuffer[Reg]
}

class Imm(imm: Int) extends Operand
{
  override def toString = imm.toString
}

class HexImm(imm: Int) extends Operand
{
  override def toString = "0x"+Integer.toHexString(imm)
}

class BaseImm(base: String, imm: Int) extends Operand
{
  override def toString =
  {
    if (imm == 0) base
    else if (imm < 0) base + imm.toString
    else base + "+" + imm.toString
  }
}

class RegImm(base: Reg, imm: Int) extends Operand
{
  override def toString = imm.toString + "(" + base + ")"
}

class RegStrImm(base: Reg, imm: String) extends Operand
{
  override def toString = imm + "(" + base + ")"
}

class AbiReg(base: Reg) extends Operand
{
  override def toString: String =
  {
    assert(base.toString.charAt(0) == 'x')
    val idx = base.toString.drop(1).toInt

    idx match
    {
      case 0  => "zero"
      case 1  => "ra"
      case 2  => "sp"
      case 3  => "gp"
      case 4  => "tp"
      case 5  => "t0"
      case 6  => "t1"
      case 7  => "t2"
      case 8  => "s0"   // aka fp
      case 9  => "s1"
      case 10 => "a0"
      case 11 => "a1"
      case 12 => "a2"
      case 13 => "a3"
      case 14 => "a4"
      case 15 => "a5"
      case 16 => "a6"
      case 17 => "a7"
      case 18 => "s2"
      case 19 => "s3"
      case 20 => "s4"
      case 21 => "s5"
      case 22 => "s6"
      case 23 => "s7"
      case 24 => "s8"
      case 25 => "s9"
      case 26 => "s10"
      case 27 => "s11"
      case 28 => "t3"
      case 29 => "t4"
      case 30 => "t5"
      case 31 => "t6"
      case _  => throw new IllegalArgumentException(s"Invalid register: $base")
    }
  }
}


class Label(val label: String) extends Operand
{
  override def toString = label
}

class PredReg(pred: Reg, neg: Boolean) extends Operand
{
  override def toString =
    if (neg) "@!" + pred
    else "@" + pred
}

// *******************************************************
class SEW(sew: Int) extends Operand
{
  override def toString = "e"+sew.toString
}

class LMUL(lmul: String) extends Operand
{
  override def toString = "m"+lmul
}

class TA() extends Operand
{
  override def toString = "ta"
}

class MA() extends Operand
{
  override def toString = "ma"
}

class V0t(b : String) extends Operand
{
  override def toString = b.toString
}

class CSR(reg : String) extends Operand
{
  override def toString = reg.toString
}

// ***********************************************************

object Imm
{
  def apply(imm: Int) = new Imm(imm)
}

object HexImm
{
  def apply(imm: Int) = new HexImm(imm)
}

object BaseImm
{
  def apply(base: String, imm: Int) = new BaseImm(base, imm)
}

object RegImm
{
  def apply(base: Reg, imm: Int) = new RegImm(base, imm)
}

object RegStrImm
{
  def apply(base: Reg, imm: String) = new RegStrImm(base, imm)
}

object AbiReg
{
  def apply(base: Reg) = new AbiReg(base)
}

object Label
{
  def apply(label: String) = new Label(label)
}

object PredReg
{
  def apply(pred: Reg, neg: Boolean) = new PredReg(pred, neg)
}

// *******************************************************
object SEW
{
  def apply(sew: Int) = new SEW(sew)
}

object LMUL
{
  def apply(lmul: String) = new LMUL(lmul)
}

object TA
{
  def apply() = new TA()
}

object MA
{
  def apply() = new MA()
}

object V0t
{
  def apply(b : String) = new V0t(b)
}

object CSR
{
  def apply(reg : String) = new CSR(reg)
}
// *******************************************************
