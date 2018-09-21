package io.pjan.retry

import cats.Eq
import scala.concurrent.duration._

final case class RetryContext(
    nrOfRetries: Int,
    cumulativeDelay: FiniteDuration,
    previousDelay: Option[FiniteDuration]
) { self =>
  def next(delay: FiniteDuration): RetryContext =
    RetryContext.next(self, delay)

  def next(delay: Option[FiniteDuration]): RetryContext =
    delay match {
      case Some(d) => RetryContext.next(self, d)
      case None    => RetryContext.next(self, Duration.Zero)
    }
}

object RetryContext {
  val Zero: RetryContext =
    RetryContext(0, Duration.Zero, None)

  def next(ctx: RetryContext, delay: FiniteDuration): RetryContext =
    RetryContext(
      nrOfRetries = ctx.nrOfRetries + 1,
      cumulativeDelay = ctx.cumulativeDelay + delay,
      previousDelay = Some(delay)
    )

  implicit val eq: Eq[RetryContext] =
    Eq.fromUniversalEquals[RetryContext]
}
