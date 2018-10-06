package battleship.core.models

import scala.annotation.tailrec
import scala.collection.immutable.Seq
import scala.util.Random

trait Player {

  val name: String
  val ships: Seq[Ship]
  val shots: Map[(Int, Int), Boolean]
  val receivedShots: Seq[(Int, Int)]
  val numberOfWins: Int
  val random: Random

  /**
    *
    * @param gridSize
    * @return
    */
  def shoot(gridSize: Int): (Int, Int)

  /**
    *
    * @param shot
    * @return
    */
  def receiveShoot(shot: (Int, Int)): (Player, Boolean, Option[Ship])

  /**
    *
    * @return
    */
  def numberOfShipsLeft(): Int = {

    @tailrec
    def numberOfShipsLeftTR(ships: Seq[Ship], number: Int): Int = {
      val currentShip = ships.headOption
      currentShip match {
        case None => number
        case Some(ship) => if (ship.isSunk()) numberOfShipsLeftTR(ships.tail, number) else numberOfShipsLeftTR(ships.tail, number + 1)
      }
    }

    numberOfShipsLeftTR(ships, 0)
  }

  /**
    *
    * @param target
    * @param didTouch
    * @return
    */
  def didShoot(target: (Int, Int), didTouch: Boolean): Player

  /**
    *
    * @return
    */
  def addVictory(): Player

  /**
    * Lolllosd
    *
    * @param shipsConfig
    * @param gridSize
    * @return
    */
  def reset(shipsConfig: Map[String, Int], gridSize: Int): Player

}

object Player {

  val LEFT = 1
  val UP = 2
  val RIGHT = 3
  val DOWN = 4

  def nextSlot(origin: (Int, Int), slot: (Int, Int), rayon: Int, direction: Int): ((Int, Int), Int, Boolean) = {
    slot match {
      case _ if origin._1 - rayon == slot._1 && origin._2 == slot._2 => {
        ((slot._1 - 1, slot._2 - 1), UP, true)
      }
      case _ => {
        direction match {
          case LEFT => {
            if (origin._1 - rayon == slot._1 && origin._2 + rayon == slot._2)
              ((slot._1, slot._2 - 1), UP, false)
            else
              ((slot._1 - 1, slot._2), LEFT, false)
          }
          case UP => {
            if (origin._1 - rayon == slot._1 && origin._2 - rayon == slot._2)
              ((slot._1 + 1, slot._2), RIGHT, false)
            else
              ((slot._1, slot._2 - 1), UP, false)
          }
          case RIGHT => {
            if (origin._1 + rayon == slot._1 && origin._2 - rayon == slot._2)
              ((slot._1, slot._2 + 1), DOWN, false)
            else
              ((slot._1 + 1, slot._2), RIGHT, false)
          }
          case _ => {
            if (origin._1 + rayon == slot._1 && origin._2 + rayon == slot._2)
              ((slot._1 - 1, slot._2), LEFT, false)
            else
              ((slot._1, slot._2 + 1), DOWN, false)
          }
        }
      }
    }
  }

  @tailrec
  def findClosestFreeSlot(origin: (Int, Int), shots: Map[(Int, Int), Boolean], rayon: Int, slot: (Int, Int), direction: Int, gridSize: Int): (Int, Int) = {
    if (
      slot._1 >= gridSize ||
        slot._1 < 0 ||
        slot._2 >= gridSize ||
        slot._2 < 0 ||
        shots.contains(slot)
    ) {
      val newSlotAndDirection = nextSlot(origin, slot, rayon, direction)
      findClosestFreeSlot(origin, shots, if (newSlotAndDirection._3) rayon + 1 else rayon, newSlotAndDirection._1, newSlotAndDirection._2, gridSize)
    } else {
      slot
    }
  }
}