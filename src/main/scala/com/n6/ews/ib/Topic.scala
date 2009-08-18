package com.n6.ews.ib

import scala.actors._

case class Subscribe(a: Actor)
case class Unsubscribe(a: Actor)
case class Publish(msg: Any)

trait Topic extends Actor {
  def act() = actWithSubscribers(List())

  def actWithSubscribers(subs: List[Actor]): Nothing = {
    react {
      case Subscribe(newSubscriber: Actor) => actWithSubscribers(newSubscriber :: subs)
      case Unsubscribe(oldSubscriber: Actor) => actWithSubscribers(subs - oldSubscriber)
      case 'exit => subs.map { _ ! 'exit }
      case Publish(msg: Any) => subs.map { _ forward msg }; actWithSubscribers(subs)
    }
  }
}

object Topic extends Topic {
  def apply() = start()
}
