package battleship.core

import battleship.core.models.Ship._

import scala.collection.immutable.ListMap

case class GameConfig() {

  /**
    * The configuration of the ships of the game
    */
  val shipsConfig: Map[String, Int] = ListMap(Map(
    CARRIER -> 5,
    BATTLESHIP -> 4,
    CRUISER -> 3,
    SUBMARINE -> 3,
    DESTROYER -> 2
  ).toSeq.sortBy(-_._2): _*)

  /**
    * The size of the grid
    */
  val gridSize: Int = 10
}
