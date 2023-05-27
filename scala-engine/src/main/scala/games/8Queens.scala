package org.gengine
package games
import org.jpl7.*

import scala.annotation.tailrec
import scala.util.matching.Regex

/* ------------------------------------------------ UTILITY FUNCTIONS ------------------------------------------------ */

def directedCheck(board: Array[Array[String]], currRow: Int, currCol: Int, rowOffset: Int, colOffset: Int) : Boolean = {
  try {
    // Return the current cell is empty AND the next checks are all true
    board(currRow)(currCol) == "0" &&
      directedCheck(board, currRow + rowOffset, currCol + colOffset, rowOffset, colOffset)
  } catch {
    case _: ArrayIndexOutOfBoundsException => true
  }
}

def queenClearCheck(board: Array[Array[String]], row: Int, col: Int) : Boolean = {
  // up-left && up && up-right && left && right && down-left && down && down-right
  directedCheck(board, row-1, col-1, -1, -1) && directedCheck(board, row-1, col, -1, 0) &&
    directedCheck(board, row-1, col+1, -1, 1) && directedCheck(board, row, col-1, 0, -1) &&
    directedCheck(board, row, col+1, 0, 1) && directedCheck(board, row+1, col-1, 1, -1) &&
    directedCheck(board, row+1, col, 1, 0) && directedCheck(board, row+1, col+1, 1, 1)
}

/* --------------------------------------------------- CONTROLLER --------------------------------------------------- */

def eightQueensController(currState: GameState, input: String) : (GameState, Boolean) = {
  val move: String = input.replaceAll(" ", "")
  val putPattern: Regex = "^[pP][uU][tT]([1-8])([a-hA-H])$".r
  val removePattern: Regex = "^[rR][eE][mM][oO][vV][eE]([1-8])([a-hA-H])$".r
  val solvePattern: Regex = "^[sS][oO][lL][vV][eE]$".r
  val board = currState._1

  move match {
    case putPattern(r,c) =>
      val (row,col) = (getRow(r,8), getCol(c.charAt(0)))
      // If there's no queen there, and the new queen's position
      // won't compromise existing queens, accept the move.
      if (board(row)(col) == "0" && queenClearCheck(board, row, col)) {
        board(row)(col) = "1"
        return ((board,1),true)
      }
      // Otherwise, reject the move.
      (currState,false)

    case removePattern(r,c) =>
      val (row,col) = (getRow(r,8), getCol(c.charAt(0)))
      // If there is a queen in the target position, accept the move.
      if (board(row)(col) == "1") {
        board(row)(col) = "0"
        return ((board, 0), true)
      }
      // Otherwise, reject the move.
      (currState,false)

    case solvePattern(_*) =>
      val(solvedState, solvable) = eightQueensSolve(currState)
      if(solvable){
        print("Solution found!")
        (solvedState,true)
      }else{
        print("The queens currently placed make it impossible to solve the puzzle!" +
          "\nTry to remove or replace some pieces.")
        (currState,true)
      }

    case _ => (currState, false)
  }
}

/* ----------------------------------------------------- DRAWER ----------------------------------------------------- */

def draw8QueensPiece(piece: String) = piece match{
  case "1" => s"\u2002${Console.BLACK}\u265B\u2002"
  case "0" => "\u2002\u2003\u2002"
  case x: String => x
}

def eightQueensDrawer(currState: GameState) : Unit = {
  println("\n\n\n")
  for (row <- currState._1.indices) {
    print(Console.RED + (8 - row) + " " + Console.RESET)
    for (col <- currState._1.indices) {
      print((if ((row + col) % 2 == 0) "\u001b[48;5;172m" else "\u001b[48;5;130m") + 
        draw8QueensPiece(currState._1(row)(col)) + "\u001b[0m")
    }
    println()
  }
  print("   ")
  for (col <- currState._1.indices) {
    print(Console.RED + ('A' + col).toChar + "  \u2009")
  }
  println(Console.RESET)
}

/* ----------------------------------------------------- SOLVER ----------------------------------------------------- */

def eightQueensSolve(gameState: GameState): (GameState, Boolean) = {
  val q1 = new Query("consult('src/main/prolog/8queens.pl')") // Link to Prolog file
  if(!q1.hasSolution) { // Check that the Prolog file works
    print("Problem with prolog file, cannot consult.")
    return (gameState, false)
  }
  // Construct input string to Prolog.
  val PlInput: String = getPlState(0, gameState._1)
  val q2 = new Query("queens( Q ),labeling( [ff],Q ),Q=["+PlInput+"].")
  if(q2.hasSolution) { // If there is a solution, get it and parse it to a game state and return it with true.
    val queenPositions = q2.oneSolution().get("Q").toString.replaceAll("[\\[\\]]", "").split(", ").map(_.toInt)
    val solvedBoard = eightQueensStartGenerator()._1
    placeQueens(0, queenPositions, solvedBoard)
    ((solvedBoard,0),true)
  }else{ // If there is no solution, return the same state with false.
    (gameState,false)
  }
}

// Applies solution to an empty board
@tailrec
def placeQueens(col: Int, positions: Array[Int], board: Array[Array[String]]) : Unit = {
  if (col == 8) {return}
  board(8-positions(col))(col) = "1"
  placeQueens(col+1, positions, board)
}

// Construct input from a board.
def getPlState(currCol: Int, board: Array[Array[String]]) : String = {
  if(currCol == 7)
    getColQueen(0, currCol, board)
  else
    getColQueen(0, currCol, board) ++ "," ++ getPlState(currCol+1, board)
}

// In a single column, if there is a queen, get the index of the row it is in,
// and if there isn't any queens, return _
@tailrec
def getColQueen(currRow: Int, col: Int, board: Array[Array[String]]) : String = {
  if(currRow == 8) return "_"
  if(board(currRow)(col) == "1")
    (8-currRow).toString
  else
    getColQueen(currRow+1, col, board)
}