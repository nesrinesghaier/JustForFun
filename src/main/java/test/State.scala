package test

import java.util.ArrayList


class State(var F: Boolean, var W: Boolean, var G: Boolean, var C: Boolean) {

  def eq(F1: Boolean, W1: Boolean, G1: Boolean, C1: Boolean): Boolean =
    if (F == F1 & W == W1 & G == G1 & C == C1) true else false

  def printSt(): Unit = {
    println(F + "\t" + W + "\t" + G + "\t" + C)
  }
}

object Farmer {

  var path: ArrayList[State] = new ArrayList[State]()

  def main(args: Array[String]): Unit = {
    solve(false, false, false, false)
    println("The Farmer-Wolf-Goat-Cabbage riddle solution")
    println("---------------------------")
    for (i <- 0 until path.size) {

      if (i > 0) {
        print("cross N°" + i + "   ")
        if (path.get(i).F && !path.get(i - 1).F) print("The farmer goes with")
        if (path.get(i).G && !path.get(i - 1).G) print(" the goat\n")
        if (path.get(i).W && !path.get(i - 1).W) print(" the wolf\n")
        if (path.get(i).C && !path.get(i - 1).C) print(" the gabbage\n")
        if (path.get(i - 1).F && !path.get(i).F && path.get(i - 1).G && !path.get(i).G) print("The farmer returns with the goat\n")
        else if(path.get(i - 1).F && !path.get(i).F)println("The farmer returns alone")
      }else{
      println("The farmer, the goat, the wolf and the gabbage are on the left side of the river")
      }
    }
    println("The farmer, the goat, the wolf and the gabbage are now on the right side of the river")
  }

  def solve(F: Boolean, W: Boolean, G: Boolean, C: Boolean): Boolean = {
    var done: Boolean = false
    if (F && W && G && C) {
      path.add(new State(F, W, G, C))
      return true
    } else if (illegal(F, W, G, C)) {
      false
    } else if (visited(F, W, G, C)) {
      false
    } else {
      path.add(new State(F, W, G, C))
      if (!done & F) done = solve(false, W, G, C)
      if (!done & !F) done = solve(true, W, G, C)
      if (!done & F & W) done = solve(false, false, G, C)
      if (!done & !F & !W) done = solve(true, true, G, C)
      if (!done & F & G) done = solve(false, W, false, C)
      if (!done & !F & !G) done = solve(true, W, true, C)
      if (!done & F & C) done = solve(false, W, G, false)
      if (!done & !F & !C) done = solve(true, W, G, true)
      done
    }
  }

  def illegal(F: Boolean, W: Boolean, G: Boolean, C: Boolean): Boolean = {
    if (F && !W && !G) return true
    if (!F && W && G) return true
    if (F && !G && !C) return true
    if (!F && G && C) return true
    return false
  }

  def visited(F: Boolean, W: Boolean, G: Boolean, C: Boolean): Boolean = {
    var ok: Boolean = false
    for (i <- 0 until path.size if path.get(i).eq(F, W, G, C)) ok = true
    ok
  }

}

