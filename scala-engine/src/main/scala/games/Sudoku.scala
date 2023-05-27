package org.gengine
package games
import java.util.regex.Pattern
import scala.annotation.tailrec
import scala.util.matching.Regex
import org.jpl7._

/* ------------------------------------------------ UTILITY FUNCTIONS ------------------------------------------------ */

@tailrec
def checkVerticalExistence(gameState: GameState, value: String, col: Int, row: Int): Boolean = {
  if(row >= gameState._1.length)
    return false
  if(gameState._1(row)(col) == value)
    return true
  checkVerticalExistence(gameState, value, col, row + 1)
}

@tailrec
def applySolution(gameState: Array[Array[String]], solution: Array[Array[Int]], row: Int, col: Int): Unit = {
  if(gameState(row)(col) == " ")
    gameState(row)(col) = solution(row)(col).toString
  if (row + col == 16)
    return ()
  applySolution(gameState, solution, (row + (col / 8)) % 9, (col + 1) % 9)
}
/* --------------------------------------------------- CONTROLLER --------------------------------------------------- */

def sudokuController(gameState: GameState, move: String) : (GameState, Boolean) = {

  val position = """(?i)(\b(?:\d[a-i])|(?:[a-i]\d)\b)(?-i)""".r.unanchored
  val operation = """(?i)(del|\b\d\b|solve)(?-i)""".r.unanchored

  def delete(pos: Array[Int])(gameState: GameState): (GameState, Boolean) = {
    if (gameState._1(pos(0))(pos(1)).length == 1) {
      return (gameState, false)
    }
    val newState = copyState(gameState._1)
    newState(pos(0))(pos(1)) = " "
    ((newState, gameState._2), true)
  }

  def insert(pos: Array[Int], value: String)(gameState: GameState): (GameState, Boolean) = {
    val NW_corner_currentSquare: Array[Int] = pos.map(p => (p / 3) * 3)

    var invalidOP: Boolean = false

    if(gameState._1(pos(0))(pos(1)) != " ")
      invalidOP = true

    gameState._1.slice(NW_corner_currentSquare(0), NW_corner_currentSquare(0) + 3)
      .map(row => row.slice(NW_corner_currentSquare(1), NW_corner_currentSquare(1) + 3))
      .foreach(row => {
        if (row.contains(value))
          invalidOP = true
      })

    if(gameState._1(pos(0)).contains(value)) invalidOP = true

    if(checkVerticalExistence(gameState, value, pos(1), 0)) invalidOP = true

    if(invalidOP)
      return (gameState, false)

    val newState = copyState(gameState._1)

    newState(pos(0))(pos(1)) = s"${value}u"
    ((newState, gameState._2), true)
  }

  move match
    case operation(first) => first.toLowerCase() match
      case "solve" => return sudokuSolve(gameState)
      case _ => ()

  val pos =  (move match
    case position(first) => first
    case _ => return (gameState, false)
    ).map( //use regular expressions to find the required input
    c => { //mapping the string to an indexed sequence of ints representing the x and y coordinates of position of input
      (
        if (c.isLetter) {
          c.toLower - 'a'
        } else
          c - 49
        ) match { //pattern matching output of if condition to make sure that the input is within acceptable range
        case x: Int if x >= gameState._1.length => return (gameState, false)
        case y => y
      }
    }
  ) match { //mapping the output indexed sequence to an array and returning false if no indexed sequence of length 2 is found
    case IndexedSeq(x: Int, y: Int) => Array(y, x)
    case _ => return (gameState, false)
  }

  val op = (
    move match
    case operation(first) => first.toLowerCase() match
      case "del" => delete(pos)
      case "solve" => sudokuSolve
      case value: String if 0 to 9 contains value.toInt => insert(pos, value)
    case _ => return (gameState, false)
    )

  op(gameState)
}

/* ----------------------------------------------------- DRAWER ----------------------------------------------------- */

def sudokuDrawer(gameState: GameState): Unit = {
  val border: String = s"${Console.BLACK_B}\u001b[38;5;54m┃"
  val tJoint: String = s"\u001b[38;5;54m╋${Console.RESET}${Console.BLACK_B}"
  val sideJoint: String = s"\u001b[38;5;54m┣${Console.RESET}${Console.BLACK_B}"
  println("  " + s"${Console.BLACK_B}\u001b[38;5;54m┏━━━┳━━━┳━━━┳━━━┳━━━┳━━━┳━━━┳━━━┳━━━┓${Console.RESET}")
  for (row <- gameState._1.indices) {
    print(Console.GREEN + (row + 1) + " " + Console.RESET)
    for (col <- gameState._1(0).indices) {
      if(col % 3 == 0)
        print(s"${border}${Console.RESET}")
      print(s"${Console.BLACK_B}${Console.MAGENTA} ${gameState._1(row)(col)(0) match
        case '0' => ' '
        case x => x
      } ${Console.RESET}")
      if((col+1) % 3 != 0)
        print(s"${Console.BLACK_B}│${Console.RESET}")
    }
    if((row + 1) % 3 != 0)
      print(s"${border}${Console.RESET}" +
        s"\n  ${Console.BLACK_B}${sideJoint}───┼───┼───${tJoint}───┼───┼───${tJoint}───┼───┼───")
    if ((row + 1) % 3 == 0 && (row + 1) != gameState._1.length)
      print(s"${border}${Console.RESET}" +
        s"\n  ${Console.BLACK_B}\u001b[38;5;54m┣━━━╋━━━╋━━━╋━━━╋━━━╋━━━╋━━━╋━━━╋━━━")
    println(s"${Console.BLACK_B}\u001b[38;5;54m┃${Console.RESET}")
  }
  println("  " + s"${Console.BLACK_B}\u001b[38;5;54m┗━━━┻━━━┻━━━┻━━━┻━━━┻━━━┻━━━┻━━━┻━━━┛${Console.RESET}")
  print("    " + Console.GREEN)
  for (col <- gameState._1(0).indices) {
    print((col + 'A').toChar + "   ")
  }
  println(Console.RESET)
}

/* ----------------------------------------------------- SOLVER ----------------------------------------------------- */

def sudokuSolve(gameState: GameState): (GameState, Boolean) = {
  val boardToString: Array[Array[String]] = Array.fill(9, 9)("_")

  for(i <- 0 to 8)
    for(j <- 0 to 8)
      if(gameState._1(i)(j) != " ")
        boardToString(i)(j) = gameState._1(i)(j)(0).toString

  val boardTS = boardToString.map(row => "[" + row.mkString(", ") + "]").mkString(", ")
  val finalString = "[" + boardTS + "]"

  val q1 = new Query("consult('src/main/prolog/SudokuSolver.pl')")

  q1.hasSolution

  val q = Query("Rows = " + finalString + ", sudoku(Rows), maplist(label,Rows).")

  if(!q.hasSolution)
    return (gameState, false)
  val result = q.oneSolution().toString

  val rowsString = result.replaceAll("Rows=|\\{|\\}", "").stripPrefix("[[").stripSuffix("]]")
  val rows = rowsString.split("\\], \\[")
  val arrayElements = rows.map(array => array.split(", "))
  val solvedSudoku = arrayElements.map(_.map(_.toInt))
  val newState = copyState(gameState._1)
  applySolution(newState, solvedSudoku, 0, 0)
  ((newState, gameState._2 + 1), true)
}