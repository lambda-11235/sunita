lazy val root = (project in file(".")).
  settings(
    name := "units",
    version := "1.0"
  )

scalacOptions in (Compile,doc) ++= Seq("-groups", "-implicits")