package io.pjan.retry.syntax

import cats.{ Monad, MonadError }
import cats.effect.Timer

import io.pjan.retry._

final class RetryStatement[F[_], A](
    statement: F[A],
    policy: RetryPolicy[F]
) {

  def on[E](
      shouldRetry: Either[E, A] => Boolean
  )(
      implicit
      M: MonadError[F, E],
      T: Timer[F]
  ): F[A] =
    Retry.on[F, A, E](
      retryPolicy = policy,
      shouldRetry = _ => { ea: Either[E, A] => M.pure(shouldRetry(ea)) },
      action = _ => statement
    )

  def onResult(
      shouldRetry: A => Boolean
  )(
      implicit
      M: Monad[F],
      T: Timer[F]
  ): F[A] =
    Retry.onResult[F, A](
      retryPolicy = policy,
      shouldRetry = _ => { a: A => M.pure(shouldRetry(a)) },
      action = _ => statement
    )

  def onError[E](
      shouldRetry: E => Boolean
  )(
      implicit
      M: MonadError[F, E],
      T: Timer[F]
  ): F[A] =
    Retry.onError[F, A, E](
      retryPolicy = policy,
      shouldRetry = _ => { e: E => M.pure(shouldRetry(e)) },
      action = _ => statement
    )

  def onAllErrors[E](
      implicit
      M: MonadError[F, E],
      T: Timer[F]
  ): F[A] =
    Retry.onError[F, A, E](
      retryPolicy = policy,
      shouldRetry = _ => { _: E => M.pure(true) },
      action = _ => statement
    )
}
