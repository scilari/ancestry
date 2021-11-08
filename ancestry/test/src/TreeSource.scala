package com.scilari.ancestry.core

object TreeSource {
  val tree: Tree[String] =
    Branch(
      "R",
      List(
        Branch(
          "A",
          List(
            Leaf("x"),
            Branch("y", List(Leaf("1"), Leaf("2"), Leaf("3"))),
            Leaf("z")
          )
        ),
        Branch("B", List(Leaf("w"))),
        Branch(
          "C",
          List(
            Branch(
              "D",
              List(
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
