package com.n6.ews.bb

import actors.Actor
import actors.Actor._
import reflect.Manifest

case class Update(newValue: Any, manifest: Manifest[_])

object Blackboard {
  val board = new Blackboard start

  def put[T](value: T)(implicit m: Manifest[T]) = board ! Put(value, m)
  def take[T](f: (T) => Boolean)(implicit m: Manifest[T]): List[T] = (board !? Take(f, m)).asInstanceOf[List[T]]
  def assignSupervisor(sup: Actor) = board ! AssignSupervisor(sup) 

  private case class AssignSupervisor(sup: Actor)
  private case class Put[T](value: T, manifest: Manifest[T])
  private case class Take[T](f: (T) => Boolean, manifest: Manifest[T])

  lazy private val Sink = actor { loop { receive { case _ => true } } }

  class Blackboard extends Actor {
    import scala.collection.immutable.Map

    private var supervisor: Actor = Sink
    private var values = Map.empty[Any, Manifest[_]]

    def act() {
      loop {
        receive {
          case AssignSupervisor(sup) =>
            supervisor = sup

          case Put(value: Any, m: Manifest[_]) =>
            values = values + (value -> m)
            supervisor ! Update(value, m)

          case Take(f, manifest) =>
            val matches = for {
              k <- values.keys.toList
              k_manifest = values(k)
              if (k_manifest <:< manifest)
              if f(k)
            } yield k

            reply(matches)
        }
      }
    }
  }
}
