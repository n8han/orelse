object Test {
  type PF = PartialFunction[String, Boolean]
  val pf: PF = {
    case "hello" => true
  }
  val fallback: PF = {
    case _ => false
  }
  val opt = folder(orElse)_
  val std = folder(stdOrElse)_

  def folder(f: (PF, PF) => PF)(n: Int) =
    f((pf /: (1 to n)) {
      (a,i) => f(a, pf)
    }, fallback)

  def alot(block: => Any) {
    for (_ <- 1 to 100000) block
  }
  def time(block: => Any) = {
    val start = System.currentTimeMillis
    block
    System.currentTimeMillis - start
  }
  def stdOrElse[A, B, A1 <: A, B1 >: B](
    left: PartialFunction[A,B],
    right: PartialFunction[A1, B1]
  ) = left.orElse(right)

  def orElse[A, B, A1 <: A, B1 >: B](
    left: PartialFunction[A,B],
    right: PartialFunction[A1, B1]
  ): PartialFunction[A1, B1] =
    new OrElse(asAttempt(left), asAttempt(right))

  def asAttempt[A,B](pf: PartialFunction[A,B]): PartialAttempt[A,B] =
    pf match {
      case pa: PartialAttempt[_,_] => pa
      case pf => new AttemptWrapper(pf)
    }

  trait PartialAttempt[-A,+B] extends PartialFunction[A,B] {
    def attempt(x: A): Option[B]
  }
  class AttemptWrapper[-A,+B](underlying: PartialFunction[A,B])
  extends PartialAttempt[A,B] {
    val lifted = underlying.lift
    def isDefinedAt(x: A) = underlying.isDefinedAt(x)
    def apply(x: A) = underlying.apply(x)
    def attempt(x: A) = lifted(x)
  }
  class OrElse[A,B,A1 <: A, B1 >: B](
    left: PartialAttempt[A,B],
    right: PartialAttempt[A1,B1]
  ) extends PartialAttempt[A1,B1] {
    def isDefinedAt(x: A1): Boolean = {
      left.isDefinedAt(x) || right.isDefinedAt(x)
    }
    def apply(x: A1): B1 = { 
      left.attempt(x) orElse {
        right.attempt(x)
      } getOrElse {
        throw new MatchError(x)
      }
    }
    def attempt(x: A1): Option[B1] = { 
      left.attempt(x).orElse {
        right.attempt(x)
      }
    }
  }
  def main(args: Array[String]) {
    def test(n: Int) {
      val List(st1, st2, ot1, ot2) = 
        for { f <- std(n) :: opt(n) :: Nil
              str <- "hello" :: "hell" :: Nil
        } yield time { alot { f(str) } }

      println("n=%3d std: %4d, %-4d opt: %4d, %-4d".format(
        n, st1, st2, ot1, ot2))
    }
    (1 to 5).foreach(test)
    println("^^^^ warmup lap ^^^^")
    (1 to 50).foreach(test)
  }
}
