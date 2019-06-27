import org.scalatest.{FlatSpec, Matchers}

class FizzBuzzSpec extends FlatSpec with Matchers {
  "The FizzBuzz function" should "return a string with containing the number 1 when passed the integer 1" in {
    FizzBuzz.execute(List(1)) shouldBe 
    """1
      |fizz: 0 buzz: 0 fizzbuzz: 0 integer: 1""".stripMargin
  }

  it should "return a string with containing the word fizz when passed the integer 3" in {
    FizzBuzz.execute(List(3)) shouldBe 
    """fizz
      |fizz: 1 buzz: 0 fizzbuzz: 0 integer: 0""".stripMargin
  }

  it should "return a string with containing the word fizz when passed a number that is a multiple of 3" in {
    FizzBuzz.execute(List(9)) shouldBe 
    """fizz
      |fizz: 1 buzz: 0 fizzbuzz: 0 integer: 0""".stripMargin
  }

  it should "return a string with containing the word buzz when passed the integer 5" in {
    FizzBuzz.execute(List(5)) shouldBe 
    """buzz
      |fizz: 0 buzz: 1 fizzbuzz: 0 integer: 0""".stripMargin
  }

  it should "return a string with containing the word buzz when passed a number that is a multiple of 5" in {
    FizzBuzz.execute(List(10)) shouldBe 
    """buzz
      |fizz: 0 buzz: 1 fizzbuzz: 0 integer: 0""".stripMargin
  }

  it should "return a string with containing the word fizzbuzz when passed the integer 15" in {
    FizzBuzz.execute(List(15)) shouldBe 
    """fizzbuzz
      |fizz: 0 buzz: 0 fizzbuzz: 1 integer: 0""".stripMargin
  }

  it should "return a string with containing the word fizzbuzz when passed a number that is a multiple of 15" in {
    FizzBuzz.execute(List(45)) shouldBe 
    """fizzbuzz
      |fizz: 0 buzz: 0 fizzbuzz: 1 integer: 0""".stripMargin
  }

  it should "return a string like `1 2 fizz 4 buzz` when passed a list of ints like [1,2,3,4,5]" in {
    FizzBuzz.execute((1 to 5).toList) shouldBe
    """1 2 fizz 4 buzz
      |fizz: 1 buzz: 1 fizzbuzz: 0 integer: 3""".stripMargin
  }

  it should "fulfil the requirement for part one of the test" in {
    FizzBuzz.execute((1 to 20).toList) shouldBe
    """1 2 fizz 4 buzz fizz 7 8 fizz buzz 11 fizz 13 14 fizzbuzz 16 17 fizz 19 buzz
      |fizz: 5 buzz: 3 fizzbuzz: 1 integer: 11""".stripMargin
  }

  it should "not allow negative number" in {
    FizzBuzz.execute(List(-5, -4, -3, -2, -1)) shouldBe "Negative numbers are not valid for this function"
  }

  it should "return a string containing the correct fizz buzz replacements as well as the count of replacements" in {
    val nums = (1 to 20).toList

    FizzBuzz.execute(nums) shouldBe """1 2 fizz 4 buzz fizz 7 8 fizz buzz 11 fizz 13 14 fizzbuzz 16 17 fizz 19 buzz
    |fizz: 5 buzz: 3 fizzbuzz: 1 integer: 11""".stripMargin
  }
}
