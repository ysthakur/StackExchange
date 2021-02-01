import annotation.tailrec

sealed trait Stack:
  def -:(head: Stack) = new -:(head, this)
  
  // @tailrec
  final override def toString: String =
    def helper(acc: StringBuilder, next: Stack): String =
      next match
        case head -: tail => helper(acc.append(head.toString), tail)
        case SNil => acc.append("}").toString
    
    helper(StringBuilder("{"), this)

case class -:(head: Stack, tail: Stack) extends Stack
object SNil extends Stack


def reverse(stack: Stack): Stack =
  @tailrec
  def helper(acc: Stack, curr: Stack): Stack =
    curr match
      case next -: tail => helper(next -: acc, tail)
      case SNil => acc

  helper(SNil, stack)

val topToBottom: Stack => Stack =
  case top -: rest => reverse(top -: reverse(rest))

def bottomToTop(stack: Stack): Stack =
  reverse(stack) match
    case bottom -: rest => bottom -: reverse(rest)