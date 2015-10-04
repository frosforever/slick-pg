package com.github.tminglei.slickpg

import org.scalatest.FunSuite

import scala.concurrent.Await
import scala.concurrent.duration.Duration

class CitextSupportSuite extends FunSuite {

  import ExPostgresDriver.api._

  val db = Database.forURL(url = utils.dbUrl, driver = "org.postgresql.Driver")

  case class Tab(col1: Int, col2: String)

  class Tabs(tag: Tag) extends Table[Tab](tag, "test_citext") {
    def col1 = column[Int]("COL1", O.PrimaryKey)
    def col2 = column[String]("COL2", O.SqlType("citext"))

    def * = (col1, col2) <>(Tab.tupled, Tab.unapply)
  }

  val tabs = TableQuery[Tabs]

  val compiledByCol2 = Compiled{ s: Rep[String] => tabs.filter(_.col2 === s)}

  test("citext support") {
    Await.result(db.run(
      DBIO.seq(
        tabs.schema.create,
        tabs ++= Seq(
          Tab(2, "Mixed")
        )
      ).andThen(
          DBIO.seq(
            tabs.filter(_.col2 === "mixed").result.head.map(r => assert(r === Tab(2, "Mixed"))),
            tabs.filter(_.col2 === "MiXeD").result.head.map(r => assert(r === Tab(2, "Mixed"))),
            // When compiled, the query works when the casing matches exactly...
            compiledByCol2("Mixed").result.head.map(r => assert(r === Tab(2, "Mixed"))),
            // but when queried with different casing, this should still return the same result as above but doesn't!
            compiledByCol2("MiXeD").result.head.map(r => assert(r === Tab(2, "Mixed")))
          )
        ).andFinally(
          tabs.schema.drop
        )
        .transactionally
    ), Duration.Inf)
  }

}
