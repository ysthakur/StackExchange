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

import Stack._

def reverse(stack: Stack): Stack =
  @tailrec
  def helper(acc: Stack, curr: Stack): Stack =
    curr match
      case next -: tail => helper(next -: acc, tail)
      case SNil => acc

  helper(SNil, stack)

def fold[T](init: T)(f: (T, Stack) => T)(stack: Stack): T =
  var acc = init
  var s = stack

  while s != SNil do
    val h -: t = s
    acc = f(acc, h)
    s = t

  acc


/**
 * Swap the stack at the given index with the top stack.
 * 0 swaps the second stack with the top one, 1 swaps the third
 * stack with the top one, and so on.
 */
def swapWithTop(ind: Int)(stack: Stack): Stack =
  @tailrec
  def helper(top: Stack, below: Stack, rest: Stack, ind: Int): Stack =
    rest match
      case head -: tail =>
        if ind == 0 then head -: fold(tail)((s, e) => e -: s)(below)
        else helper(top, head -: below, tail, ind - 1)

  stack match
    case h -: t => helper(h, SNil, t, ind)

val topToBottom: Stack => Stack =
  case top -: rest => reverse(top -: reverse(rest))


def bottomToTop(stack: Stack): Stack = reverse(stack) match
  case bottom -: rest => bottom -: reverse(rest)