package com.n6.ews.ib

import scala.actors.Actor._

object InfoBusTests extends Application {

  def makeEchoActor(prefix: String) =  actor {
    while(true) {
      receive {
        case 'exit => exit
        case msg => println(prefix + " received " + msg)
      }
    }
  } 

  // A dummy class, for illustration
  case class Value(value: Any)

  val e1 = makeEchoActor("One")
  val e2 = makeEchoActor("Two")
  val e3 = makeEchoActor("Three")
  val e4 = makeEchoActor("Four")

  val modelChanges = Topic()

  val displayChanges = new Value("Display Server") with Topic

  modelChanges ! Subscribe(e1)
  modelChanges ! Subscribe(e2)
  modelChanges ! Subscribe(e3)

  displayChanges ! Subscribe(e1)
  displayChanges ! Subscribe(e4)

  modelChanges ! Publish("model changed")
  modelChanges ! Publish("model changed again")

  displayChanges ! Publish("display changed")

  displayChanges.start()
  
  modelChanges ! Unsubscribe(e2)
  modelChanges ! Publish("model changed a third time")

  displayChanges ! Unsubscribe(e4)
  displayChanges ! Unsubscribe(e1)

  displayChanges ! Publish("if a tree falls in a forest...")
}
