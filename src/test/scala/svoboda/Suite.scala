package svoboda

import org.scalactic.source.Position
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.reflect.ClassTag

abstract class Suite extends AnyFlatSpec with Matchers {
  implicit class SimpleSyntax[T](private val inner: T) {
    def as[O: ClassTag](implicit pos: Position): O = {
      inner shouldBe a[O]
      inner.asInstanceOf[O]
    }
  }
}
