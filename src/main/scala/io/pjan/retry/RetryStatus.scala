package io.pjan.retry

import cats.{ Eq, Monoid }

import scala.concurrent.duration._

/**
  * A `RetryStatus` is a datastructure that captures the result of
  * applying a [[RetryContext]] to a [[RetryPolicy]].
  *
  * It can either be a [[RetryStatus.Stop]] indicating that no further
  * retries should be attempted, or a [[RetryStatus.Retry]] telling to
  * retry the failed action after a specified FiniteDuration.
  *
  * A `RetryStatus` has 2 monoid instances, so you can collapse multiple
  * status into one. The semantics of this combination are as follows:
  *
  * 1. If either status returns a [[RetryStatus.Stop]], the combined status
  * returns [[RetryStatus.Stop]]. This can be used to inhibit further retries.
  *
  * 2. If both statuses are a [[RetryStatus.Retry]], either the larger or the
  * smaller will be retained, depending on whether [[RetryStatus.MaxMonoid]] or
  * [[RetryStatus.MinMonoid]] are used respectively.
  */
sealed trait RetryStatus
object RetryStatus {
  case object Stop extends RetryStatus
  final case class Retry(
      delay: FiniteDuration
  ) extends RetryStatus

  /**
    * An `Eq` instance for `RetryStatus`
    */
  implicit final val eq: Eq[RetryStatus] =
    Eq.fromUniversalEquals[RetryStatus]

  /**
    * A `Monoid` instance for `RetryStatus` that will keep the smaller delay,
    * in case both combined statuses are a [[RetryStatus.Retry]]. In case either
    * of them is a [[RetryStatus.Stop]], the result of the combination will be a
    * [[RetryStatus.Stop]].
    */
  final val MinMonoid: Monoid[RetryStatus] =
    new Monoid[RetryStatus] {
      val empty: RetryStatus =
        RetryStatus.Retry(FiniteDuration(Long.MaxValue, NANOSECONDS))

      def combine(x: RetryStatus, y: RetryStatus): RetryStatus = (x, y) match {
        case (Retry(xd), Retry(yd)) => RetryStatus.Retry(xd min yd)
        case (_, _)                 => RetryStatus.Stop
      }
    }

  /**
    * A `Monoid` instance for `RetryStatus` that will keep the larger delay,
    * in case both combined statuses are a [[RetryStatus.Retry]]. In case either
    * of them is a [[RetryStatus.Stop]], the result of the combination will be a
    * [[RetryStatus.Stop]].
    */
  final val MaxMonoid: Monoid[RetryStatus] =
    new Monoid[RetryStatus] {
      override val empty: RetryStatus =
        RetryStatus.Retry(FiniteDuration(0L, NANOSECONDS))

      override def combine(
          x: RetryStatus,
          y: RetryStatus
      ): RetryStatus = (x, y) match {
        case (Retry(xd), Retry(yd)) => RetryStatus.Retry(xd max yd)
        case (_, _)                 => RetryStatus.Stop
      }
    }

}
