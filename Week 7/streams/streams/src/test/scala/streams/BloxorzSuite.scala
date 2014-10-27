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
  
  test("terrain function: 'o'") {
    new Level1 {
      assert(terrain(Pos(0,0)), "0,0")
    }
  }
  
  test("terrain function: 'S'") {
    new Level1 {
      assert(terrain(Pos(1,1)), "1,1")
    }
  }
  
  test("terrain function: 'T'") {
    new Level1 {
      assert(terrain(Pos(4,7)), "4,7")
    }
  }
  
  test("terrain function: '-'") {
    new Level1 {
      assert(!terrain(Pos(4,1)), "4,1")
    }
  }
  
  test("terrain function: out of bounds (x)") {
    new Level1 {
      assert(!terrain(Pos(11,1)), "11,1")
    }
  }
  
  test("terrain function: out of bounds (y)") {
    new Level1 {
      assert(!terrain(Pos(4,11)), "4,11")
    }
  }

  test("findChar level 1 start") {
    new Level1 {
      assert(startPos === Pos(1,1))
    }
  }

  test("findChar level 1 goal") {
    new Level1 {
      assert(goal === Pos(4,7))
    }
  }
  
  test("isStanding initially") {
    new Level1 {
      assert(Block(startPos, startPos).isStanding)
    }
  }
  
    test("!isStanding after move") {
    new Level1 {
      assert(!Block(startPos, startPos).right.isStanding)
    }
  }
  
  test("isLegal initially") {
    new Level1 {
      assert(Block(startPos, startPos).isLegal)
    }
  }
  
  test("!isLegal after falling off edge") {
    new Level1 {
      assert(!Block(startPos, startPos).left.isLegal)
    }
  }
  
  test("startBlock is at starting position") {
    new Level1 {
      assert(startBlock.b1 == startPos)
      assert(startBlock.b2 == startPos)
      assert(startBlock.isStanding)
      assert(startBlock.isLegal)
    }
  }
  
  test("neighbours of starting position") {
    new Level1 {
      val startNeighbours = List(
          (Block(Pos(1, -1), Pos(1, 0)), Left),
          (Block(Pos(1, 2), Pos(1, 3)), Right),
          (Block(Pos(-1, 1), Pos(0, 1)), Up),
          (Block(Pos(2, 1), Pos(3, 1)), Down)
      );
      assert(startBlock.neighbors === startNeighbours)
    }
  }
  
  test("legal neighbours of starting position") {
    new Level1 {
      val startLegalNeighbours = List(
          (Block(Pos(1, 2), Pos(1, 3)), Right),
          (Block(Pos(2, 1), Pos(3, 1)), Down)
      );
      assert(startBlock.legalNeighbors === startLegalNeighbours)
    }
  }
  
  test("not done at the start") {
    new Level1 {
      assert(!done(startBlock))
    }
  }
  
  test("done at the end") {
    new Level1 {
      assert(done(Block(goal, goal)))
    }
  }
  
  test("not done if horizontally flat at the end") {
    new Level1 {
      assert(!done(Block(goal, Pos(5,7))))
    }
  }
    
  test("not done if vertically flat at the end") {
    new Level1 {
      assert(!done(Block(Pos(4,6), goal)))
    }
  }
  
  test("neighboursWithHistory generates stream of moves") {
    new Level1 {
      val expectedSet = Set(
        (Block(Pos(1,2),Pos(1,3)), List(Right,Left,Up)),
        (Block(Pos(2,1),Pos(3,1)), List(Down,Left,Up))
      )
      
      assert(neighborsWithHistory(startBlock, List(Left, Up)).toSet === expectedSet)
    }
  }
  
  test("newNeighborsOnly generates list without loops") {
    new Level1 {
      val currentNeighbours = Set(
        (Block(Pos(1,2),Pos(1,3)), List(Right,Left,Up)),
        (Block(Pos(2,1),Pos(3,1)), List(Down,Left,Up))
      ).toStream
      
      val explored = Set(Block(Pos(1,2),Pos(1,3)), Block(Pos(1,1),Pos(1,1)));
      
      val expectedSet =  Set((Block(Pos(2,1),Pos(3,1)), List(Down,Left,Up))).toStream;
      assert(newNeighborsOnly(currentNeighbours, explored) === expectedSet)
    }
  }

  test("optimal solution for level 1") {
    new Level1 {
      assert(solve(solution) === Block(goal, goal))
    }
  }

  test("optimal solution length for level 1") {
    new Level1 {
      assert(solution.length === optsolution.length)
    }
  }
}
