package functional

object Hollywood {
  def findActor(name: String): List[Actor] = {
    List(Actor(name))
  }
}
