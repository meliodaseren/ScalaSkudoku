
object Sudoku {

  def main(args: Array[String]) = {
    //Stage 1 ..................... 
    readSudoKuFile("puzzle02.txt")
    printCells (true)

    //Stage 2 .....................
    val ans = solution.sortWith ((x, y) => x._2 < y._2 || (x._2 == y._2 && x._3 < y._3))
    //val ans = solution
    printResult (ans)
  }
  
  //Stage 1 Functions ...........................................
  val _cells = Array.ofDim[Tuple2[Int, List[Int]]](9, 9)

  def readSudoKuFile(sudokufile: String) = {
    //if a cell has value, the cell = (v, Nil), v > 0
    //if a cell has no value, the cell = (0, List (posible values))

    val lines = io.Source.fromFile(sudokufile).getLines.toArray
    val in = lines map {
      _.split(" ").toArray.map(_.toInt)
    }

    for (x <- 0 to 8;
         y <- 0 to 8) {
      in(x)(y) match {
        case 0 => _cells(x)(y) = (0, (1 to 9).toList)
        case a => _cells(x)(y) = (a, Nil)
      }
    }

    for (x <- 0 to 8;
         y <- 0 to 8;
         if (_cells(x)(y)._1 > 0)) {

      updateCells(x, y, _cells(x)(y)._1)

    }
  }
  def updateCells (x:Int, y:Int, v:Int) = {

    def updateRow (): Unit = {
      for (c <- 0 to 8; if (c !=x && _cells(c)(y)._1 == 0)) {
        _cells (c)(y) = (0, _cells (c)(y)._2.filter ( _!=v))

      }
    }

    def updateCol (): Unit = {
      for (r<- 0 to 8; if (r!= y && _cells(x)(r)._1 == 0)) {
        _cells (x)(r) = (0, _cells(x)(r)._2.filter (_!=v))
      }

    }
    def updateBox (): Unit = {
      val boxrow = y / 3
      val boxcol = x / 3

      for (c <- 0 to 2;
           r <- 0 to 2;
           row = r + boxrow*3;
           col = c + boxcol*3;
           if ((col != x || row != y) && _cells(col)(row)._1 == 0)) {

        //println (c + " " + r)
        //println (col + " " + row)
        _cells (col)(row) = (0, _cells(col)(row)._2.filter (_ != v))
      }
    }
    updateRow()
    updateCol()
    updateBox()
  }

  def printCells (detail:Boolean = false) = {
    println ()

    for (x <- 0 to 8) {
      println
      for (y <- 0 to 8) {
        print (_cells(x)(y)._1 + " ")

      }

    }

    println ()

    if (detail)
      for (x <- 0 to 8) {
        println
        for (y <- 0 to 8) {
          print (_cells(x)(y)._2 + " ")

        }

      }

    println ()

  }


  //Stage 2 Functions ..........................
  //This part of code refers to Ratul Buragohain' program.

  type Cell = (Int,Int,Int, List[Int])
  type Solutions = List[List[Cell]]



  def solution = {

    val cells = (for (x <- 0 to 8; y <- 0 to 8) yield ((_cells(x)(y)._1, x, y, _cells(x)(y)._2))).toList

    val empties = emptyCells(cells)
    val nonEmpties = nonEmptyCells(cells)
    solve(empties,nonEmpties)(0) ::: nonEmpties
    //solve(empties,nonEmpties)
  }

  def solve(empties: List[Cell], nonEmpties: List[Cell]): Solutions = {
    //Actual logic
    def fillCells(emptyCells: List[Cell]): Solutions = {
      if(emptyCells == Nil) List(List())
      else {
        for {
           lastSolutions <- fillCells(emptyCells.init)
          cellValue <- emptyCells.last._4  // 1 to 9
          cell = (cellValue,emptyCells.last._2, emptyCells.last._3, emptyCells.last._4)
          if(isOk(cell, lastSolutions ::: nonEmpties))
        } yield cell :: lastSolutions
      }
    }
    fillCells(empties)
  }
  def isOk(c:Cell, cells: List[Cell]) = {
    (colCells(c,cells) ::: rowCells(c,cells) ::: boxCells(c,cells)).forall(e => e._1 != c._1)
  }
  def colCells(c: Cell, cells: List[Cell]) = cells.filter(e => e._3 == c._3)
  def rowCells(c: Cell, cells: List[Cell]) = cells.filter(e => e._2 == c._2)
  def emptyCells(cells: List[Cell]) = cells.filter(e => e._1 == 0)
  def nonEmptyCells(cells: List[Cell]) = cells.filterNot(e => e._1 == 0)
  def boxCells(c: Cell, cells: List[Cell]) = {
    val inBox = cellAt(c)
    val filtered = inBox match {
      case 1 => cells.filter{e =>(e._2 < 3 && e._3 < 3)}
      case 2 => cells.filter{e =>(e._2 < 3 && (e._3 > 2 && e._3 < 6))}
      case 3 => cells.filter{e => (e._2 < 3 && e._3 > 5)}
      case 4 => cells.filter{e => ((e._2 > 2 && e._2 < 6) && e._3 < 3)}
      case 5 => cells.filter{e => ((e._2 > 2 && e._2 < 6) && (e._3 > 2 && e._3 < 6))}
      case 6 => cells.filter{e => ((e._2 > 2 && e._2 < 6) && (e._3 > 5))}
      case 7 => cells.filter{e => (e._2 > 5 && e._3 < 3)}
      case 8 => cells.filter{e => (e._2 > 5 && (e._3 > 2 && e._3 < 6))}
      case 9 => cells.filter{e => (e._2 > 5 && e._3 > 5)}
    }
    filtered
  }


  def cellAt(c: Cell) = c match {
    case (_,x,y,_) if(x < 3 && y < 3) => 1
    case (_,x,y,_) if (x < 3 && ( y > 2 && y < 6)) => 2
    case (_,x,y,_) if (x < 3 && (y > 5 && y < 9)) => 3
    case (_,x,y,_) if ((x > 2 && x < 6) && y < 3) => 4
    case (_,x,y,_) if ((x > 2 && x < 6) && (y > 2 && y < 6)) => 5
    case (_,x,y,_) if ((x > 2 && x < 6) && (y > 5 && y < 9)) => 6
    case (_,x,y,_) if (x > 5 && y < 3) => 7
    case (_,x,y,_) if (x > 5 && (y > 2 && y < 6)) => 8
    case _ => 9
  }

  def printResult (ans: List[Cell]) = {

    for (cell <- ans) {print (cell._1 + " "); if (cell._3 == 8) println}

  }
}


