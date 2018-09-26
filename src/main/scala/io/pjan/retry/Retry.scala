package io.pjan.retry

import cats.{ Functor, Monad, MonadError }
import cats.effect.Timer
import cats.syntax.applicative._
import cats.syntax.applicativeError._
import cats.syntax.apply._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.option._

object Retry {

  def onResult[F[_]: Monad: Timer, A](
      retryPolicy: RetryPolicy[F],
      shouldRetry: RetryContext => A => F[Boolean],
      action: RetryContext => F[A]
  ): F[A] = {
    def go(ctx: RetryContext): F[A] =
      action(ctx).flatMap { r =>
        shouldRetry(ctx)(r).ifM(
          for {
            nextCtxO <- nextContextWithDelay(retryPolicy, ctx)
            result <- nextCtxO match {
                       case None          => r.pure[F]
                       case Some(nextCtx) => go(nextCtx)
                     }
          } yield result,
          r.pure[F]
        )
      }

    go(RetryContext.Zero)
  }

  def onError[F[_]: MonadError[?[_], E]: Timer, A, E](
      retryPolicy: RetryPolicy[F],
      shouldRetry: RetryContext => E => F[Boolean],
      action: RetryContext => F[A]
  ): F[A] = {
    def go(ctx: RetryContext): F[A] =
      action(ctx).handleErrorWith { e =>
        shouldRetry(ctx)(e).ifM(
          for {
            nextCtxO <- nextContextWithDelay(retryPolicy, ctx)
            result <- nextCtxO match {
                       case None          => e.raiseError[F, A]
                       case Some(nextCtx) => go(nextCtx)
                     }
          } yield result,
          e.raiseError[F, A]
        )
      }

    go(RetryContext.Zero)
  }

  def on[F[_]: MonadError[?[_], E]: Timer, A, E](
      retryPolicy: RetryPolicy[F],
      shouldRetry: RetryContext => Either[E, A] => F[Boolean],
      action: RetryContext => F[A]
  ): F[A] =
    onResult(retryPolicy, shouldRetry, ctx => action(ctx).attempt).flatMap {
      case Right(result) => result.pure[F]
      case Left(e)       => e.raiseError[F, A]

    }

  private def nextContext[F[_]: Functor](
      retryPolicy: RetryPolicy[F],
      ctx: RetryContext
  ): F[Option[RetryContext]] =
    retryPolicy.run(ctx).map {
      case RetryStatus.Retry(delay) => ctx.next(delay).some
      case RetryStatus.Stop         => none
    }

  private def nextContextWithDelay[F[_]: Monad: Timer](
      retryPolicy: RetryPolicy[F],
      ctx: RetryContext
  ): F[Option[RetryContext]] =
    nextContext(retryPolicy, ctx).flatMap {
      case Some(nextCtx @ RetryContext(_, _, Some(d))) => implicitly[Timer[F]].sleep(d) *> nextCtx.some.pure[F]
      case Some(nextCtx @ RetryContext(_, _, None))    => nextCtx.some.pure[F]
      case None                                        => none.pure[F]
    }

}
