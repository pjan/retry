package io.pjan.retry

import cats.{ Functor, Monad, MonadError }
import cats.effect.Timer
import cats.syntax.either._
import cats.syntax.functor._
import cats.syntax.flatMap._
import cats.syntax.apply._
import cats.syntax.applicativeError._

object Retry {

  def onResult[F[_], A](
      retryPolicy: RetryPolicy[F],
      shouldRetry: RetryContext => A => F[Boolean],
      action: RetryContext => F[A]
  )(
      implicit
      M: Monad[F],
      T: Timer[F]
  ): F[A] = {
    def go(ctx: RetryContext): F[A] =
      for {
        _result <- action(ctx)
        _shouldRetry <- shouldRetry(ctx)(_result)
        result <- if (_shouldRetry) {
                   for {
                     nextCtxO <- nextContextWithDelay(retryPolicy, ctx)
                     result <- nextCtxO match {
                                case None          => M.pure(_result): F[A]
                                case Some(nextCtx) => go(nextCtx): F[A]
                              }
                   } yield result
                 } else {
                   M.pure(_result): F[A]
                 }
      } yield result

    go(RetryContext.Zero)
  }

  def onError[F[_], A, E](
      retryPolicy: RetryPolicy[F],
      shouldRetry: RetryContext => E => F[Boolean],
      action: RetryContext => F[A]
  )(
      implicit
      M: MonadError[F, E],
      T: Timer[F]
  ): F[A] = {
    def go(ctx: RetryContext): F[A] =
      action(ctx).handleErrorWith { e =>
        for {
          _shouldRetry <- shouldRetry(ctx)(e)
          result <- if (_shouldRetry) {
                     for {
                       nextCtxO <- nextContextWithDelay(retryPolicy, ctx)
                       result <- nextCtxO match {
                                  case None          => M.raiseError(e): F[A]
                                  case Some(nextCtx) => go(nextCtx): F[A]
                                }
                     } yield result
                   } else {
                     M.raiseError(e): F[A]
                   }
        } yield result
      }

    go(RetryContext.Zero)
  }

  def on[F[_], A, E](
      retryPolicy: RetryPolicy[F],
      shouldRetry: RetryContext => Either[E, A] => F[Boolean],
      action: RetryContext => F[A]
  )(
      implicit
      M: MonadError[F, E],
      T: Timer[F]
  ): F[A] = {
    val eitherAction: RetryContext => F[Either[E, A]] =
      ctx =>
        action(ctx).map(_.asRight[E]).handleErrorWith { e =>
          M.pure(e.asLeft[A])
        }

    onResult(retryPolicy, shouldRetry, eitherAction).flatMap {
      case Right(result) => M.pure(result)
      case Left(e)       => M.raiseError(e)
    }
  }

  private def nextContext[F[_]](
      retryPolicy: RetryPolicy[F],
      ctx: RetryContext
  )(
      implicit
      M: Functor[F]
  ): F[Option[RetryContext]] =
    retryPolicy.run(ctx).map {
      case RetryStatus.Retry(delay) => Some(ctx.next(delay))
      case RetryStatus.Stop         => None
    }

  private def nextContextWithDelay[F[_]](
      retryPolicy: RetryPolicy[F],
      ctx: RetryContext
  )(
      implicit
      M: Monad[F],
      T: Timer[F]
  ): F[Option[RetryContext]] =
    for {
      nextCtx <- nextContext(retryPolicy, ctx)
      result <- nextCtx match {
                 case Some(RetryContext(_, _, Some(d))) => T.sleep(d) *> M.pure(nextCtx)
                 case Some(RetryContext(_, _, None))    => M.pure(nextCtx)
                 case None                              => M.pure(None)
               }
    } yield result

}
