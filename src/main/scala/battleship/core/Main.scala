package battleship.core

import battleship.core.models._
import battleship.utils.io._

import scala.annotation.tailrec
import scala.util.Random

object Main extends App {

  val gameConfig = GameConfig(GameConfig.DEFAULT)
  if (gameConfig.gridSize > 10) {
    GameDisplay.gridTooBig()
    System.exit(1)
  }
  GameDisplay.clear()
  GameDisplay.introduction()
  val randoms = Seq[Random](new Random(), new Random())

  /**
    *
    * @param gameType
    * @param numberOfGames
    * @param randoms
    * @param shipConfig
    * @return
    */
  def initGameStates(numberOfGames: Int, randoms: Seq[Random], gameConfig: GameConfig): Set[GameState] = {
        GameDisplay.choseYourName(1)
        val namePlayer: String = PlayerInputs.choseName()
        val player: HumanPlayer = HumanPlayer.createPlayer(namePlayer, randoms(0), gameConfig.shipsConfig, gameConfig.gridSize)
        val ia: WeakIAPlayer = WeakIAPlayer.generateIA(1, randoms(1), gameConfig.shipsConfig, gameConfig.gridSize)
        Set(GameState(player, ia, numberOfGames, 1))
  }
  
  /**
    *
    * @param gameState
    * @param shipsConfig
    * @return
    */
  @tailrec
  def mainLoop(gameState: GameState, gameConfig: GameConfig): (Player, Player) = {
    val currentPlayer = gameState.currentPlayer
    val opponent = gameState.opponent
    val isCurrentPlayerHuman: Boolean = currentPlayer.isInstanceOf[HumanPlayer]
    val winner = gameState.isThereAWinner()


    if (winner.isEmpty) {

      if (isCurrentPlayerHuman) {
        GameDisplay.clear()
        PlayerDisplay.show(currentPlayer, opponent)
        GridDisplay.showPlayerGrid(currentPlayer.ships, opponent.shots.keys.toSeq, gameConfig.gridSize)
        GridDisplay.showOpponentGrid(currentPlayer.shots, gameConfig.gridSize)
        PlayerDisplay.shoot()
      }
      val target: (Int, Int) = currentPlayer.shoot(gameConfig.gridSize)
      val (newOpponent, touched, shipSunk): (Player, Boolean, Option[Ship]) = opponent.receiveShoot(target)
      if (isCurrentPlayerHuman) {
        if (shipSunk.isDefined) PlayerDisplay.sunk(shipSunk.get.name) else if (touched) PlayerDisplay.touched() else PlayerDisplay.notTouched()
        GameDisplay.endOfTurn()
        PlayerInputs.pressAKey()
      }
      val newCurrentPlayer = currentPlayer.didShoot(target, didTouch = touched)
      mainLoop(GameState(newOpponent, newCurrentPlayer, gameState.numberOfGames, gameState.gameCount), gameConfig)


    } else {

      val addedVictoryWinner = winner.get.addVictory()
      val continue: Boolean = if (currentPlayer.isInstanceOf[HumanPlayer] || opponent.isInstanceOf[HumanPlayer]) {
        GameDisplay.winner(addedVictoryWinner.name)
        GameDisplay.continue()
        PlayerInputs.continue() != "q"
      } else {
        gameState.gameCount < gameState.numberOfGames
      }
      if (continue) {
        GameDisplay.clear()
        GameDisplay.gameNumber(gameState.gameCount + 1, gameState.numberOfGames)
        mainLoop(GameState(currentPlayer.reset(gameConfig.shipsConfig, gameConfig.gridSize), addedVictoryWinner.reset(gameConfig.shipsConfig, gameConfig.gridSize), gameState.numberOfGames, gameState.gameCount + 1), gameConfig)
      } else {
        (currentPlayer, addedVictoryWinner)
      }

    }
  }

  val gameStates = initGameStates(100, randoms, gameConfig)
  val results = gameStates.map(gameState => {
    GameDisplay.gameNumber(gameState.gameCount, gameState.numberOfGames)
    mainLoop(gameState, gameConfig)
  })

  GameDisplay.end(results)
}
