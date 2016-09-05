package functional



/**
  * Created by spencerward on 28/08/2016.
  */
object Hollywood {
  def findActor(name: String): List[Actor] = {
    List(Actor(name))
  }
}
