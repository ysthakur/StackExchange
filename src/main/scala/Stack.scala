import annotation.tailrec

enum Stack:
  case -:(head: Stack, tail: Stack)
  case SNil

  def -:(head: Stack) = new -:(head, this)
  
  final override def toString: String =
    @tailrec
    def helper(acc: StringBuilder, next: Stack): String =
      next match
        case head -: tail => helper(acc.append(head.toString), tail)
        case SNil => acc.append("}").toString
    
    helper(StringBuilder("{"), this)