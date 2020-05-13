package svoboda

trait Alg[F[_]] {
  def get: F[Int]

  def put(i: Int): F[Unit]

  def multilist(j: String, p: Int)(k: List[Nothing]): F[String]

  def defaults(optional: Option[Int] = None): F[Int]

  def withImplementation: F[Unit] = put(100)

  def !(`type`: scala.=:=[Int, Int]): F[Unit]
}

object Alg {
  def apply[F[_]](implicit alg: Alg[F]): Alg[F] = alg
}

@toInitial[Alg]
sealed trait AlgExp[D]

class ToInitialSpec extends Suite {
  "Alg[AlgExp]" should "return correct values" in {
    implicit val alg: Alg[AlgExp] = AlgExp.instance

    alg.get shouldEqual AlgExp.get
    alg.put(1) shouldEqual alg.put(1)
    alg.put(1) shouldEqual AlgExp.put(1)
    alg.multilist("a", 1)(List.empty) shouldEqual AlgExp.multilist("a", 1, List.empty)
    alg.withImplementation shouldEqual AlgExp.put(100)
    (alg ! implicitly[Int =:= Int]).as[AlgExp.`!`].`type`.apply(1337) shouldEqual 1337
  }
}
