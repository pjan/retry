package io.pjan.retry

import cats.{ Applicative, Monad, Monoid }
import cats.data.StateT
import cats.instances.list._
import cats.syntax.applicative._
import cats.syntax.apply._
import cats.syntax.functor._
import cats.syntax.traverse._

import scala.annotation._
import scala.concurrent.duration._

/**
  * A `RetryPolicy` is a function that takes a [[RetryContext]] and
  * returns a [[RetryStatus]] on whether to retry or not, and if so,
  * after which FiniteDuration it should happen.
  *
  * A `RetryPolicy`, as a function from a [[RetryContext]] to a
  * [[RetryStatus]], has, similar to [[RetryStatus]], a min and max
  * Monoid for combining Policies, with the same semantics.
  */
trait RetryPolicy[F[_]] {
  def run: RetryContext => F[RetryStatus]
}

object RetryPolicy {
  def apply[F[_]](
      policy: RetryContext => F[RetryStatus]
  ): RetryPolicy[F] =
    new RetryPolicy[F] {
      def run: RetryContext => F[RetryStatus] =
        policy
    }

  def lift[F[_]: Applicative](
      policy: RetryContext => RetryStatus
  ): RetryPolicy[F] =
    new RetryPolicy[F] {
      def run: RetryContext => F[RetryStatus] =
        ctx => policy(ctx).pure[F]
    }

  private def monoid[F[_]: Applicative](
      rpMonoid: Monoid[RetryStatus]
  ): Monoid[RetryPolicy[F]] =
    new Monoid[RetryPolicy[F]] {
      override val empty: RetryPolicy[F] =
        RetryPolicy[F] { ctx =>
          rpMonoid.empty.pure[F]
        }

      override def combine(
          x: RetryPolicy[F],
          y: RetryPolicy[F]
      ): RetryPolicy[F] =
        RetryPolicy[F] { ctx =>
          (x.run(ctx), y.run(ctx)).mapN {
            case (xrpd, yrpd) => rpMonoid.combine(xrpd, yrpd)
          }
        }
    }

  def minMonoid[F[_]: Applicative]: Monoid[RetryPolicy[F]] =
    monoid[F](RetryStatus.MinMonoid)

  def maxMonoid[F[_]: Applicative]: Monoid[RetryPolicy[F]] =
    monoid[F](RetryStatus.MaxMonoid)

  def constantDelay[F[_]: Applicative](
      delay: FiniteDuration
  ): RetryPolicy[F] =
    lift { ctx =>
      RetryStatus.Retry(delay)
    }

  def exponentialBackoff[F[_]: Applicative](
      baseDelay: FiniteDuration
  ): RetryPolicy[F] =
    lift { ctx =>
      RetryStatus.Retry(baseDelay * scala.math.pow(2, ctx.nrOfRetries).toLong)
    }

  def fibonacciBackoff[F[_]: Applicative](
      baseDelay: FiniteDuration
  ): RetryPolicy[F] =
    lift { ctx =>
      RetryStatus.Retry(baseDelay * fib(ctx.nrOfRetries))
    }

  def fullJitterBackoff[F[_]: Applicative](
      baseDelay: FiniteDuration
  ): RetryPolicy[F] =
    lift { ctx =>
      val d      = (baseDelay * Math.pow(2, ctx.nrOfRetries).toLong) / 2
      val jitter = d * (scala.util.Random.nextDouble())
      RetryStatus.Retry(baseDelay + FiniteDuration(jitter.toNanos, NANOSECONDS))
    }

  def limitByRetries[F[_]: Applicative](
      limit: Int
  ): RetryPolicy[F] =
    lift { ctx =>
      if (ctx.nrOfRetries >= limit) RetryStatus.Stop else RetryStatus.Retry(Duration.Zero)
    }

  def limitByDelay[F[_]: Applicative](
      treshold: FiniteDuration
  )(
      retryPolicy: RetryPolicy[F]
  ): RetryPolicy[F] =
    RetryPolicy[F] { ctx =>
      retryPolicy.run(ctx).map {
        case r @ RetryStatus.Retry(d) if d <= treshold => r
        case _                                         => RetryStatus.Stop
      }
    }

  def limitByCumulativeDelay[F[_]: Applicative](
      treshold: FiniteDuration
  )(
      retryPolicy: RetryPolicy[F]
  ): RetryPolicy[F] =
    RetryPolicy[F] { ctx =>
      retryPolicy.run(ctx).map {
        case r @ RetryStatus.Retry(d) if (ctx.cumulativeDelay + d) <= treshold => r
        case _                                                                 => RetryStatus.Stop
      }
    }

  def simulate[F[_]: Monad](
      retryPolicy: RetryPolicy[F],
      nrOfRetries: Int
  ): F[List[(Int, RetryStatus)]] =
    (1 to nrOfRetries).toList
      .traverse { i =>
        StateT[F, RetryContext, (Int, RetryStatus)] { ctx =>
          retryPolicy.run(ctx).map {
            case rs @ RetryStatus.Stop         => (ctx.next(None), (i, rs))
            case rs @ RetryStatus.Retry(delay) => (ctx.next(delay), (i, rs))
          }
        }
      }
      .runA(RetryContext.Zero)

  private def fib(n: Int): Int = {
    @tailrec def go(n: Int, a: Int, b: Int): Int =
      if (n == 0) a else go(n - 1, b, a + b)

    go(n, 1, 1)
  }

}
