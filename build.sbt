

lazy val commonSettings = Seq(
  organization := "com.zackmarvel",
  version := "0.0.1"
  )

lazy val root = (project in file("."))
  .settings(commonSettings: _*)
  .settings(
    name := "bf-scala"
  )
