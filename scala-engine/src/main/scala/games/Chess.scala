package org.gengine
package games
import javax.print.attribute.standard.Destination
import scala.annotation.tailrec
import scala.util.matching.Regex

/* ------------------------------------------------ UTILITY FUNCTIONS ------------------------------------------------ */

def piece_isBlack(piece: String): Boolean = {
  piece(0) == 'B'
}

def generalChecks(black: Boolean, gameState: GameState, move: ((Int, Int),(Int, Int))): Boolean = {
  val (from, to) = move
  val toPiece = gameState._1(to._1)(to._2)
  val toPiece_isBlack: Boolean = piece_isBlack(toPiece)
  if(black ^ (gameState._2 % 2 == 0)) {
    return false
  } else if (toPiece == "  ")
    return true
  else if (!toPiece_isBlack ^ black)
    return false
  true
}

def pawn(black: Boolean)(gameState: GameState, move: ((Int, Int),(Int, Int))): Boolean = {
  val (from, to) = move
  (math.abs(to._1 - from._1) + math.abs(to._2 - from._2) <= 2 && math.abs(to._1 - from._1) > 0) &&
    (!black ^ to._1 > from._1) && (math.abs(to._1 - from._1) != 2 || ((from._1 == 1  && black) ||
    (from._1 == 6 && !black))) && (to._2 == from._2 ^ ((gameState._1(to._1)(to._2) match {
    case "  " => false
    case _ => true
  }) && (black ^ piece_isBlack(gameState._1(to._1)(to._2)))))
}

def knight(gameState: GameState, move: ((Int, Int), (Int, Int))): Boolean = {
  val (from, to) = move
  if (math.abs(to._1 - from._1) + math.abs(to._2 - from._2) != 3 || !((math.abs(to._1 - from._1) == 1) ^ (math.abs(to._2 - from._2) == 1)))
    return false
  true
}

@tailrec
def notFoundInPath(found: String => Boolean = x => x != "  ", notFound: String => Boolean = x => false)
                  (gameState: GameState, destination: (Int, Int), currentPos: (Int, Int))
                  : Boolean = {
  if(currentPos == destination || !(0 to 7 contains currentPos._1) ||
    !(0 to 7 contains currentPos._2) || notFound(gameState._1(currentPos._1)(currentPos._2)))
    return true
  if(found(gameState._1(currentPos._1)(currentPos._2)))
    return false
  notFoundInPath(found, notFound)(gameState, destination, (
      (
        (destination._1 - currentPos._1) match
          case x: Int if x < 0 => -1
          case 0 => 0
          case y => 1
      ) + currentPos._1, (
      (destination._2 - currentPos._2) match
        case x: Int if x < 0 => -1
        case 0 => 0
        case y => 1
      ) + currentPos._2
    )
  )
}

def bishop(gameState: GameState, move: ((Int, Int), (Int, Int))): Boolean = {
  val (from, to) = move
  if (math.abs(to._1 - from._1) != math.abs(to._2 - from._2))
    return false

  notFoundInPath()(gameState, to, (from._1 + (if(from._1 > to._1) -1 else 1),
                    from._2 + (if (from._2 > to._2) -1 else 1)))
}

def rook(gameState: GameState, move: ((Int, Int), (Int, Int))): Boolean = {
  val (from, to) = move
  if (math.abs(to._1 - from._1) != 0 && math.abs(to._2 - from._2) != 0)
    return false

  notFoundInPath()(gameState, to, (from._1 + (if (from._1 == to._1) 0 else if(from._1 > to._1) -1 else 1),
                    from._2 + (if (from._2 == to._2) 0 else if (from._2 > to._2) -1 else 1)))
}

def blank(gameState: GameState, move: ((Int, Int), (Int, Int))): Boolean = {
  false
}

def queen(gameState: GameState, move: ((Int, Int), (Int, Int))): Boolean = {
  rook(gameState, move) || bishop(gameState, move)
}

def king(black: Boolean)(gameState: GameState, move: ((Int, Int), (Int, Int))): Boolean = {
  val (from, to) = move
  if (math.abs(to._1 - from._1) != 1 && math.abs(to._2 - from._2) != 1)
    return false

  def horseCheck(row: Int, col: Int): Boolean = !((0 to 7 contains row) && (0 to 7 contains col) &&
                                                  gameState._1(row)(col)(1) == 'N' &&
                                                  (gameState._1(row)(col)(0) == 'B' ^ black))

  val safe_diagonal = notFoundInPath(x => (x(1) == 'Q' ||
                                     x(1) == 'B') && (x(0) == 'B' ^ black), x => (x != "  ") && (x(0) == 'B' ^ !black))

  val safe_polar = notFoundInPath(x => (x(1) == 'Q' || x(1) == 'R') &&
                                  (x(0) == 'B' ^ black), x => (x != "  ") && (x(0) == 'B' ^ !black))

  !(!horseCheck(to._1 + 1, to._2 + 2) ||
    !horseCheck(to._1 + 1, to._2 - 2) ||
    !horseCheck(to._1 - 1, to._2 + 2) ||
    !horseCheck(to._1 - 1, to._2 - 2) ||
    !horseCheck(to._1 + 2, to._2 + 1) ||
    !horseCheck(to._1 + 2, to._2 - 1) ||
    !horseCheck(to._1 - 2, to._2 + 1) ||
    !horseCheck(to._1 - 2, to._2 - 1) ||

    !safe_diagonal(gameState, (to._1 + Math.min(8 - to._1, 8 - to._2), to._2 + Math.min(8 - to._1, 8 - to._2)), to) ||
    !safe_diagonal(gameState, (to._1 + Math.min(8 - to._1, to._2 + 1), to._2 - Math.min(8 - to._1, to._2 + 1)), to) ||
    !safe_diagonal(gameState, (to._1 - Math.min(to._1 + 1, 8 - to._2), to._2 + Math.min(to._1 + 1, 8 - to._2)), to) ||
    !safe_diagonal(gameState, (to._1 - Math.min(to._1 + 1, to._2 + 1), to._2 - Math.min(to._1 + 1, to._2 + 1)), to) ||

    !safe_polar(gameState, (to._1, 8), to) ||
    !safe_polar(gameState, (to._1, -1), to) ||
    !safe_polar(gameState, (8, to._2), to) ||
    !safe_polar(gameState, (-1, to._2), to)
  )
}

def translate_input(move: Array[String]): ((Int, Int), (Int, Int)) = {
  val translated_input: Array[IndexedSeq[Int]] = new Array[IndexedSeq[Int]](2)
  for (i <- move.indices)
    translated_input(i) = move(i).map(c => {
      if (c.isLetter)
        c.toLower - 'a'
      else
        56 - c
    })
  (
    (translated_input(0)(1), translated_input(0)(0)),
    (translated_input(1)(1), translated_input(1)(0))
  )
}

/* --------------------------------------------------- CONTROLLER --------------------------------------------------- */

def chessController(gameState: GameState, move: String): (GameState, Boolean) = {
  val (from, to) = translate_input({
    val moves = (new Regex("[a-hA-H]\\d") findAllIn move).take(2).toArray
    if (moves.length != 2)
      return (gameState, false)
    else
      moves
  })
  val piece: (GameState, ((Int, Int), (Int, Int))) => Boolean = gameState._1(from._1)(from._2) match {
    case "WP" => pawn(black = false)
    case "WN" => knight
    case "WB" => bishop
    case "WK" => king(black = false)
    case "WQ" => queen
    case "WR" => rook
    case "BP" => pawn(black = true)
    case "BN" => knight
    case "BB" => bishop
    case "BK" => king(black = true)
    case "BQ" => queen
    case "BR" => rook
    case _ => blank
  }

  if(!piece(gameState, (from, to)) ||
    !generalChecks(piece_isBlack(gameState._1(from._1)(from._2)), gameState, (from, to)))
    return (gameState, false)

  val newState: Array[Array[String]] = gameState._1.map(row => row.map(identity))
  assert(newState.length == 8 && newState(0).length == 8)
  newState(to._1)(to._2) = gameState._1(from._1)(from._2)
  newState(from._1)(from._2) = "  "
  ((newState, 3 - gameState._2), true)
}

/* ----------------------------------------------------- DRAWER ----------------------------------------------------- */

def drawChessPiece(piece: String) = piece match{
  case "WR" => "\u2002\u265C\u2002"
  case "WN" => "\u2002\u265E\u2002"
  case "WB" => "\u2002\u265D\u2002"
  case "WQ" => "\u2002\u265B\u2002"
  case "WK" => "\u2002\u265A\u2002"
  case "WP" => "\u2002\u2659\u2002"
  case "BR" => s"\u2002${Console.BLACK}\u265C\u2002"
  case "BN" => s"\u2002${Console.BLACK}\u265E\u2002"
  case "BB" => s"\u2002${Console.BLACK}\u265D\u2002"
  case "BQ" => s"\u2002${Console.BLACK}\u265B\u2002"
  case "BK" => s"\u2002${Console.BLACK}\u265A\u2002"
  case "BP" => s"\u2002${Console.BLACK}\u2659\u2002"
  case "  " => "\u2002\u2003\u2002"
  case x: String => x
}

def chessDrawer(gameState: GameState): Unit = {
  println(Console.GREEN + "\n\nPlayer " + gameState._2 + "'s Turn " +
    (if (gameState._2 == 1) "(White)" else "(Black)") + ":\n")
  for(row <- gameState._1.indices) {
    print(Console.RED + (8 - row) + " " + Console.RESET)
    for(col <- gameState._1.indices) {
      print((if((row + col) % 2 == 0) "\u001b[48;5;172m" else "\u001b[48;5;130m") +
        drawChessPiece(gameState._1(row)(col)) + "\u001b[0m")
    }
    println()
  }
  print("   ")
  for(col <- gameState._1.indices) {
    print(Console.RED + ('A' + col).toChar + "  \u2009")
  }
  println(Console.RESET)
}
