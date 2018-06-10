
### Local Debugging

The example shows how to use Gameplay class to test your bot TestBot.
The Gameplay class contains the game logic, in particular `parseString`, `createMatch`, `step`
and other functions, so the developer can debug the bot logic locally without the server.

```scala
object SBot extends App {
  val random = new Random(123)
  val gameplay = new Gameplay
  val bots = mutable.Buffer[Bot](new SBot("1", random))
  //val bots = mutable.Buffer[Bot](new SBot("1", random), new SBot("2", random))
  val botNames = bots.map(_.getName).asJava
  val mgs = gameplay.createMatch(10, 20, bots.asJava, 100L, 0.9, 0).getGameState
  for (_ ← 0 until 100) {
    for (k ← bots.indices) {
      val gs = gameplay.getClientGameState(mgs, k)
      val move = bots(k).move(gs)
      gameplay.step(mgs, k, move)
      println("move = " + move + " current game state = \n" +
        gameplay.describeGameState(mgs, botNames, false, false))
      Thread.sleep(100)
    }
  }
}
```
