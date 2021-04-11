import Stack._
import annotation.tailrec

def concat(stack1: Stack, stack2: Stack) =
  foldRight(stack2)(_ -: _)(stack1)

def reverse(stack: Stack): Stack =
  @tailrec
  def helper(acc: Stack, curr: Stack): Stack =
    curr match
      case next -: tail => helper(next -: acc, tail)
      case SNil => acc

  helper(SNil, stack)

def foldLeft(init: Stack)(f: (Stack, Stack) => Stack)(stack: Stack): Stack =
  var acc = init
  var s = stack

  while s != SNil do
    val h -: t = s
    acc = f(acc, h)
    s = t

  acc

def foldRight(init: Stack)(f: (Stack, Stack) => Stack)(stack: Stack): Stack =
  foldLeft(init)((acc, s) => f(s, acc))(reverse(stack))

/**
 * Swap the stack at the given index with the top stack.
 * 0 swaps the second stack with the top one, 1 swaps the third
 * stack with the top one, and so on.
 */
def swapWithTop(ind: Int)(stack: Stack): Stack =
  @tailrec
  def helper(top: Stack, below: Stack, rest: Stack, ind: Int): Stack =
    (rest: Stack @unchecked) match
      case head -: tail =>
        if ind == 0 then head -: foldLeft(tail)((s, e) => e -: s)(below)
        else helper(top, head -: below, tail, ind - 1)

  stack match
    case h -: t => helper(h, SNil, t, ind)
    case singletonStack => singletonStack


val topToBottom: Command =
  case top -: rest => reverse(top -: reverse(rest))
  case singletonStack => singletonStack


def bottomToTop(stack: Stack): Stack = reverse(stack) match
  case bottom -: rest => bottom -: reverse(rest)
  case singletonStack => singletonStack


/**
 * TODO Fix, potential source of stack overflows
 * Find the difference between two stacks, recursively
 */
def stackDiff(s1: Stack, s2: Stack): Stack = (s1, s2) match
  case (SNil, s) => s
  case (s, SNil) => s
  case (a -: s, b -: t) => stackDiff(a, b) -: stackDiff(s, t)

def maxByLen(s1: Stack, s2: Stack): Stack = ???