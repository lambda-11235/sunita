lazy val root = (project in file(".")).
  settings(
    name := "units",
    version := "0.1.0"
  )


scalacOptions in (Compile) ++= Seq("-deprecation", "-feature")

scalacOptions in (Compile,doc) ++= Seq("-groups", "-implicits")