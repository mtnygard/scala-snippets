package com.n6.ews.bb

import org.specs.runner.{JUnitSuiteRunner, JUnit4, JUnit}
import org.specs.Specification


object BlackboardSpecs extends Specification with JUnit {
  "The Blackboard " should {
    "store and retrieve Any values" in {
      Blackboard put "A string"
      Blackboard put 42
      Blackboard put 1234567890L
      Blackboard put "Firstname Lastname Address"
      Blackboard put 1.0

      val smallIntegers = Blackboard.take[Int] { n => n < 1000000 }

      smallIntegers must contain(42)
      smallIntegers must haveSize(1)

      val strings = Blackboard.take[String] { _ => true }


      strings must haveSize(2)
      strings must contain("A string")
      strings must contain("Firstname Lastname Address")

      Blackboard put "Another string"

      val aStrings = Blackboard.take[String] { _.startsWith("A") }


      aStrings must haveSize(2)
      aStrings must contain("A string")
      aStrings must contain("Another string")
    }
  
    "store and retrieve case classes" in {
      case class Evidence(items: List[Int])
      case class Person(firstname: String, lastname: String) {
        def mathematician = firstname.startsWith("Leo")
      }

      Blackboard put Evidence(List(1, 1, 3, 5, 8, 13, 21, 35))
      Blackboard put Person("Leonardo", "Pisano")
      Blackboard put Person("Leonardo", "da Vinci")
      Blackboard put Person("Leonhard", "Euler")
      Blackboard put Person("Wilhelm", "Wien")

      val numbers = Blackboard.take[Evidence] { _ => true }

      numbers must haveSize(1)
      numbers(0).items must haveSize(8)

      val names = Blackboard.take[Person] { _.mathematician }


      names must haveSize(3)
      
      names foreach { n =>
        n must verify ( _.firstname.startsWith("Leon") )
      }
    }
  }
}
