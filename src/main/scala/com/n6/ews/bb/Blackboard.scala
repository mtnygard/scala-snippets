package com.n6.ews.bb

object Blackboard {
  import scala.reflect.Manifest
  import scala.collection.immutable.Map

  private var values = Map.empty[Any, Manifest[_]]

  def put[T](value: T)(implicit m: Manifest[T]) = { 
    values = values + (value -> m) 
  }

  def take[T](f: (T) => Boolean)(implicit m: Manifest[T]): List[T] =
    for {
      k <- values.keys.toList
      if (values(k) <:< m)
      value = k.asInstanceOf[T]
      if f(value)
    } yield value
}
