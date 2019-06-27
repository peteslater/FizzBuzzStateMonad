import cats.data.State
import cats.implicits._

object FizzBuzz {
  case class FizzBuzzResults(fizz: Int = 0,
                             buzz: Int = 0,
                             fizzbuzz: Int = 0,
                             integer: Int = 0)

  type FizzBuzzState[A] = State[FizzBuzzResults, A]

  def execute(nums: List[Int]): String = {
    if (nums.forall(_ >= 0)) {
      val (state, result) = convertNumbers(nums)

      (result.mkString(" ") :+ scala.util.Properties.lineSeparator :+ buildReport(state)).mkString
    }
    else {
      "Negative numbers are not valid for this function"
    }
  }

  private def buildReport(state: FizzBuzzResults): String = {
    s"fizz: ${state.fizz} buzz: ${state.buzz} fizzbuzz: ${state.fizzbuzz} integer: ${state.integer}"
  }

  private def convertNumbers(nums: List[Int]): (FizzBuzzResults, List[String]) = nums.traverse[FizzBuzzState, String](num => convertNumber(num)).run(FizzBuzzResults()).value

  private def convertNumber(i: Int): State[FizzBuzzResults, String] = State(state => {
    i match {
      case n if n % 15 == 0 => (state.copy(fizzbuzz = state.fizzbuzz + 1), "fizzbuzz")
      case n if n % 5 == 0 => (state.copy(buzz = state.buzz + 1), "buzz")
      case n if n % 3 == 0 => (state.copy(fizz = state.fizz + 1), "fizz")
      case _ => (state.copy(integer = state.integer + 1), i.toString)
    }
  })
}
