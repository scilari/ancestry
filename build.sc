import mill._, scalalib._
import mill.scalajslib.ScalaJSModule

object ancestry extends ScalaModule with ScalaJSModule {
  def scalaVersion = "3.3.3"
  def scalaJSVersion = T { "1.16.0" }

  def publishVersion = "0.0.1"

  object test extends ScalaTests with TestModule.ScalaTest {
    override def ivyDeps =
      Agg(
        ivy"com.lihaoyi::utest::0.7.10",
        ivy"org.scalatest::scalatest:3.2.10",
        ivy"org.scalacheck::scalacheck:1.15.4"
      )

  }
}
