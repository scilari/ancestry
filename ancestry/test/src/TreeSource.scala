package com.scilari.ancestry.core

object TreeSource {
  val tree: Tree[String] =
    Branch(
      "R",
      Seq(
        Branch(
          "A",
          Seq(
            Leaf("x"),
            Branch("y", Seq(Leaf("1"), Leaf("2"), Leaf("3"))),
            Leaf("z")
          )
        ),
        Branch("B", Seq(Leaf("w"))),
        Branch(
          "C",
          Seq(
            Branch(
              "D",
              Seq(
                Leaf("p"),
                Leaf("q"),
                Leaf("r")
              )
            )
          )
        )
      )
    )
}
