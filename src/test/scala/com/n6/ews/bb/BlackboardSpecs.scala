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

      val smallIntegers = Blackboard take {
        case n: Int => n < 1000000
        case _ => false
      }

      smallIntegers must haveSize(1)
      smallIntegers must contain(42)

      val strings = Blackboard take {
        case s: String => true
        case _ => false
      }

      strings must haveSize(2)
      strings must contain("A string")
      strings must contain("Firstname Lastname Address")

      Blackboard put "Another string"

      val aStrings = Blackboard take {
        case s: String => s.startsWith("A")
        case _ => false
      }

      aStrings must haveSize(2)
      aStrings must contain("A string")
      aStrings must contain("Another string")
    }
  }
}