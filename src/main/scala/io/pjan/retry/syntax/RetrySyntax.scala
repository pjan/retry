package io.pjan.retry.syntax

import io.pjan.retry._

trait RetrySyntax {
  implicit def retrySyntax[F[_], A](fa: F[A]): RetryOps[F, A] =
    new RetryOps[F, A](fa)
}

final class RetryOps[F[_], A](val self: F[A]) extends AnyVal {

  def retryWith(
      policy: RetryPolicy[F]
  ): RetryStatement[F, A] =
    new RetryStatement(self, policy)

}
