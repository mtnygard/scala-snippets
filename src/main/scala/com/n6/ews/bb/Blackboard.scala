package com.n6.ews.bb


import scala.actors.Actor
import scala.actors.Actor._

object Blackboard {

  def put(value: Any) = board ! Put(value)
  def take(f: PartialFunction[Any, Boolean]): List[Any] = {
    board !? Take(f) match {
      case l: List[Any] => l
      case _ => List()
    }
  }

  /* messages to the blackboard */
  case class Take(template: Any)
  case class Put(value: Any)

  private val board = new Board start

  private class Board extends Actor {
    var values: List[Any] = List()

    def act() = loop {
      receive {
        case Put(value) =>
          value match {
            case list: List[Any] => values = list ::: values
            case single: Any => values = single :: values
          }

        case Take(f: PartialFunction[Any, Boolean]) =>
          val matches = values.filter(f)
          reply(matches)
      }
    }
  }
}