package streams

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import Bloxorz._

@RunWith(classOf[JUnitRunner])
class BloxorzSuite extends FunSuite {

  trait SolutionChecker extends GameDef with Solver with StringParserTerrain {
    /**
     * This method applies a list of moves `ls` to the block at position
     * `startPos`. This can be used to verify if a certain list of moves
     * is a valid solution, i.e. leads to the goal.
     */
    def solve(ls: List[Move]): Block =
      ls.foldLeft(startBlock) { case (block, move) => move match {
        case Left => block.left
        case Right => block.right
        case Up => block.up
        case Down => block.down
      }
    }
  }

  trait Level1 extends SolutionChecker {
      /* terrain for level 1*/

    val level =
    """ooo-------
      |oSoooo----
      |ooooooooo-
      |-ooooooooo
      |-----ooToo
      |------ooo-""".stripMargin
    
    val optsolution = List(Right, Right, Down, Right, Right, Right, Down)
  }
  
  trait Level4 extends SolutionChecker {

    val level =
    """ooo--
      |oSooo
      |ooooo
      |-oooo
      |--ooT""".stripMargin
    
      val optsolution = List(Right, Right, Down, Down)
  }
  
  trait Level2 extends SolutionChecker {

    val level =
    """ST-
      |oo-
      |oo-""".stripMargin
    
      val optsolution = List(Down, Right, Up)
  }
  
  trait Level3 extends SolutionChecker {

    val level =
    """oooo
      |oSoT
      |oooo""".stripMargin
    
      val optsolution = List(Down, Right, Right, Up)
  }

  test("terrain function level 1") {
    new Level1 {
      assert(terrain(Pos(0,0)), "0,0")
      assert(!terrain(Pos(4,11)), "4,11")
      assert(terrain(startPos), "startPos should be true")
      assert(terrain(goal), "goal should be true")
      assert(!terrain(Pos(0,9)), "0,9 should be false")
    }
  }

  test("findChar level 1") {
    new Level1 {
      assert(startPos == Pos(1,1))
      assert(startBlock == Block(startPos, startPos))
      assert(goal === Pos(4,7))
    }
  }

  test("legal level 1") {
    new Level1 {
      assert(Block(Pos(0, 0), Pos(0, 0)).isLegal, "0,0 should be true")
      assert(!Block(Pos(0, 9), Pos(0, 9)).isLegal, "0,9 should be false")
      assert(Block(startPos, startPos).isLegal, "startPos should be true")
      assert(Block(goal, goal).isLegal, "goal should be true")
      assert(!Block(Pos(4, 11), Pos(4, 11)).isLegal, "4,11 should be false")
    }
  }

  test("neighbors level 1") {
    new Level1 {
      val h = startBlock.neighbors
      val r = Set(
        (Block(Pos(-1, 1), Pos(0, 1)), Up),
        (Block(Pos(2, 1), Pos(3, 1)), Down),
        (Block(Pos(1, -1), Pos(1, 0)), Left),
        (Block(Pos(1, 2), Pos(1, 3)), Right))
      assert(h.toSet === r)
    }
  }

  test("neighbors above goal level 1") {
    new Level1 {
      val h = Block(Pos(2, 7), Pos(3, 7)).neighbors
      val r = Set(
        (Block(Pos(2, 6), Pos(3, 6)), Left),
        (Block(Pos(2, 8), Pos(3, 8)), Right),
        (Block(Pos(1, 7), Pos(1, 7)), Up),
        (Block(Pos(4, 7), Pos(4, 7)), Down))
      assert(h.toSet === r)
    }
  }

  test("legalNeighbors level 1") {
    new Level1 {
      val h = startBlock.legalNeighbors
      val r = Set(
        (Block(Pos(1, 2), Pos(1, 3)), Right),
        (Block(Pos(2, 1), Pos(3, 1)), Down))
      assert(h.toSet === r)
    }
  }

  test("legalNeighbors above goal level 1") {
    new Level1 {
      val h = Block(Pos(2, 7), Pos(3, 7)).legalNeighbors
      val r = Set(
        (Block(Pos(2, 6), Pos(3, 6)), Left),
        (Block(Pos(2, 8), Pos(3, 8)), Right),
        (Block(Pos(4, 7), Pos(4, 7)), Down))
      assert(h.toSet === r)
    }
  }

  test("neighbors with history level 1") {
    new Level1 {
      val h = neighborsWithHistory(Block(Pos(1, 1), Pos(1, 1)), List(Left, Up))
      val r = Set(
        (Block(Pos(1, 2), Pos(1, 3)), List(Right, Left, Up)),
        (Block(Pos(2, 1), Pos(3, 1)), List(Down, Left, Up)))
      assert(h.toSet === r)
    }
  }

  test("new neighbors only level 1") {
    new Level1 {
      val h = newNeighborsOnly(
        Set(
          (Block(Pos(1, 2), Pos(1, 3)), List(Right, Left, Up)),
          (Block(Pos(2, 1), Pos(3, 1)), List(Down, Left, Up))).toStream,

        Set(Block(Pos(1, 2), Pos(1, 3)), Block(Pos(1, 1), Pos(1, 1))))
      val r = Set(
        (Block(Pos(2, 1), Pos(3, 1)), List(Down, Left, Up))).toStream
      assert(h === r)
    }
  }
  
  test("one step before optimal goal level 1") {
	  new Level1 {
		  val allResults = Set(
				  (Block(Pos(2,6),Pos(3,6)),List(Left, Right, Right, Right, Down, Right, Right)),
				  (Block(Pos(2,8),Pos(3,8)),List(Right, Right, Right, Right, Down, Right, Right)),
				  (Block(Pos(4,7),Pos(4,7)),List(Down, Right, Right, Right, Down, Right, Right))
				  ).toStream

				  val n = neighborsWithHistory(Block(Pos(2,7),Pos(3,7)),List(Right, Right, Right, Down, Right, Right))
				  assert(n === allResults)

				  val h = `newNeighborsOnly`(n, Set(Block(Pos(2,6),Pos(3,6))))
				  val newResults = Set(
						  (Block(Pos(2,8),Pos(3,8)),List(Right, Right, Right, Right, Down, Right, Right)),
						  (Block(Pos(4,7),Pos(4,7)),List(Down, Right, Right, Right, Down, Right, Right))
						  ).toStream
						  assert(h === newResults)
	  }
  }

  
  test("optimal solution for level 2") {
    new Level2 {
      assert(goal === Pos(0,1))
      assert(solve(solution) == Block(goal, goal))
    }
  }
  
  test("optimal solution for level 3") {
    new Level3 {
      assert(goal === Pos(1,3))
      assert(solve(solution) == Block(goal, goal))
    }
  }
    
  test("optimal solution for level 4") {
    new Level4 {
      assert(goal === Pos(4,4))
      assert(solve(solution) == Block(goal, goal))
    }
  }
  
  test("optimal solution for level 1") {
    new Level1 {
      assert(goal === Pos(4,7))
      assert(solve(solution) == Block(goal, goal))
    }
  }

  test("optimal solution length for level 1") {
    new Level1 {
      assert(solution.length == optsolution.length)
    }
  }
  
  
  test( "newNeighborsOnly" ) {
	  new Level1 {
		  
		  val ns = newNeighborsOnly(
				  Set(
					  (Block(Pos(1,2),Pos(1,3)), List(Right,Left,Up)),
					  (Block(Pos(2,1),Pos(3,1)), List(Down,Left,Up))
					  ).toStream,
					  
				  Set(Block(Pos(1,2),Pos(1,3)), Block(Pos(1,1),Pos(1,1)))
				  )
		  val expectedNs =  Set( (Block(Pos(2,1),Pos(3,1)), List(Down,Left,Up))).toStream
		  
		  assert( expectedNs == ns )
	  }
  }
}
