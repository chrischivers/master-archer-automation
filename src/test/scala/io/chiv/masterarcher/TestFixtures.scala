package io.chiv.masterarcher
import java.util.UUID

import cats.effect.{ContextShift, IO, Resource}
import doobie.h2.H2Transactor
import doobie.util.transactor.Transactor
import io.chiv.masterarcher.persistence.{PostgresStore, Store}
import org.scalatest.Assertion

import scala.concurrent.ExecutionContext

trait TestFixtures {

  private def h2Transactor(implicit ec: ExecutionContext,
                           contextShift: ContextShift[IO]): Resource[IO, H2Transactor[IO]] =
    H2Transactor
      .newH2Transactor[IO](
        url = s"jdbc:h2:mem:${UUID.randomUUID()};DB_CLOSE_DELAY=-1;DATABASE_TO_UPPER=false",
        user = "sa",
        pass = "",
        connectEC = ec,
        transactEC = ec
      )

  def withDb(f: Transactor[IO] => IO[Assertion])(implicit ec: ExecutionContext, contextShift: ContextShift[IO]) = {
    h2Transactor.use(f(_)).unsafeRunSync()
  }

  def withPostgresStore(f: Store => IO[Assertion])(implicit ec: ExecutionContext, contextShift: ContextShift[IO]) =
    h2Transactor.use(db => PostgresStore(db).flatMap(f)).unsafeRunSync()

}
