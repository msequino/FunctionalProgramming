package streams

import org.junit._
import org.junit.Assert.assertEquals

import Bloxorz._

class BloxorzSuite {
  trait SolutionChecker extends GameDef with Solver with StringParserTerrain {
    /**
     * This method applies a list of moves `ls` to the block at position
     * `startPos`. This can be used to verify if a certain list of moves
     * is a valid solution, i.e. leads to the goal.
     */
    def solve(ls: List[Move]): Block =
      ls.foldLeft(startBlock) { case (block, move) =>
        require(block.isLegal) // The solution must always lead to legal blocks
        move match {
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


  @Test def `terrain function level 1 (10pts)`: Unit =
    new Level1 {
      assert(terrain(Pos(0,0)), "0,0")
      assert(terrain(Pos(1,1)), "1,1") // start
      assert(terrain(Pos(4,7)), "4,7") // goal
      assert(terrain(Pos(5,8)), "5,8")
      assert(!terrain(Pos(5,9)), "5,9")
      assert(terrain(Pos(4,9)), "4,9")
      assert(!terrain(Pos(6,8)), "6,8")
      assert(!terrain(Pos(4,11)), "4,11")
      assert(!terrain(Pos(-1,0)), "-1,0")
      assert(!terrain(Pos(0,-1)), "0,-1")
    }

  @Test def `find char level 1 (10pts)`: Unit =
    new Level1 {
      assertEquals(Pos(1, 1), startPos)
    }


  @Test def `optimal solution for level 1 (5pts)`: Unit =
    new Level1 {
      assertEquals(Block(goal, goal), solve(solution))
    }


  @Test def `optimal solution length for level 1 (5pts)`: Unit =
    new Level1 {
      assertEquals(optsolution.length, solution.length)
    }


  @Rule def individualTestTimeout = new org.junit.rules.Timeout(10 * 1000)

  @Test def `findChar level 1`() : Unit = {
    new Level1 {
      assert(startPos == Pos(1,1))
    }
  }

  @Test def `findChar goal` {
    new Level1 {
      assert(goal == Pos(4,7))
    }
  }

  @Test def `test legaltity start block` {
    new Level1 {
      assert(startBlock.isLegal)
    }
  }

  @Test def `test neighbors of start block` {
    new Level1 {
      assert(startBlock.neighbors.length == 4)
      assert(startBlock.legalNeighbors.length == 2)
    }
  }

  @Test def `test legaltity moves around borders` {
    new Level1 {
      val moved = startBlock.down
      assert(moved.legalNeighbors.length == 2)
    }
  }

  @Test def `test from start go to up` {
    new Level1 {
      val moved = startBlock.up
      assert(moved.legalNeighbors.length == 1)
    }
  }

  @Test def `test legality from the center of the terrain` {
    new Level1 {
      val moved = startBlock.right.right.down
      assert(moved.legalNeighbors.length == 3)
    }
  }

  @Test def `test neighbors with history` {
    new Level1 {
      val b = Block(Pos(1,1),Pos(1,1))
      val moves = List(Left,Up)
      val neighbors = neighborsWithHistory(b, moves).toList

      val sol = LazyList(
        (Block(Pos(2,1),Pos(3,1)), List(Down,Left,Up)),
        (Block(Pos(1,2),Pos(1,3)), List(Right,Left,Up))
      ).toList

      assert(neighbors.forall(n => sol.contains(n)))

    }
  }

  @Test def `test avoiding circles` {
    new Level1 {
      val neighbors = LazyList(
        (Block(Pos(1, 2), Pos(1, 3)), List(Right, Left, Up)),
        (Block(Pos(2, 1), Pos(3, 1)), List(Down, Left, Up))
      )

      val explored = Set(
        Block(Pos(1, 2), Pos(1, 3)), Block(Pos(1, 1), Pos(1, 1))
      )

      val newNeighborsOnlySol = newNeighborsOnly(neighbors, explored)

      val sol = LazyList(
        (Block(Pos(2, 1), Pos(3, 1)), List(Down, Left, Up))
      )

      for {
        (n, ni) <- newNeighborsOnlySol.zipWithIndex
        (s, si) <- sol.zipWithIndex if si == ni
      } yield {
        println(s._1, n._1)
        assert(s._1.equals(n._1))
      }
    }
  }

  @Test def `optimal solution for level 1` {
    new Level1 {
      println("start")
      assert(solve(solution) == Block(goal, goal))
    }
  }


  @Test def `optimal solution length for level 1` {
    new Level1 {
      assert(solution.length == optsolution.length)
    }
  }
}
