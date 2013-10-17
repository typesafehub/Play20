package play.api.libs.iteratee.frx

import akka.frx._
import play.api.libs.iteratee._
import scala.concurrent.{ ExecutionContext, Future, Promise }
import scala.annotation.tailrec

// FIXME: Variance
trait ExclusiveObserver[In] extends Observer[In] {

  protected def queueProcessingContext: ExecutionContext // Assume prepared
  private var processing = false
  private val queue = new java.util.concurrent.LinkedBlockingQueue[(In, SubscriptionOutput[In])]

  final def apply(in: In, sub: SubscriptionOutput[In]): Boolean = {
    val startProcessing = synchronized {
      queue.add((in, sub))
      if (processing) {
        false
      } else {
        processing = true
        true
      }
    }
    if (startProcessing) {
      processQueue()
    }
    false
  }

  private def processQueue() {
    implicit val ec = queueProcessingContext
    val next = queue.poll()
    if (next == null) {
      // The queue is empty; possibly stop
      synchronized {
        if (queue.isEmpty) {
          processing = false
        } else {
          // Some input has arrived, try again
          processQueue()
        }
      }      
    } else {
      val (in, sub) = next
      exclusiveApply(in, sub).onComplete(_ => processQueue())
    }
  }

  protected def exclusiveApply(in: In, sub: SubscriptionOutput[In]): Future[Unit]
}

class ClosableSubscriptions[In](private var _open: Boolean) {

  def open: Boolean = synchronized { _open }

  var subs: List[SubscriptionOutput[In]] = Nil

  def close(/* reason */) {
    synchronized { _open = false }
    for (sub <- subs) sub.close(/* reason? */)
  }

  def addInput(sub: SubscriptionOutput[In]) {
    val closeSub = synchronized {
      if (open) {
        subs = subs :+ sub
        false
      } else {
        true
      }
    }
    // Perform close outside synchronized block, in case it takes time
    if (closeSub) sub.close() // Does subscription code handle immediate close?
  }

  def removeInput(sub: SubscriptionOutput[In]) {
    // What should we do if the sub doesn't exist?
    synchronized { subs = subs.filter(_ != sub) }
  }

}

case class IterateeErrorException[E](msg: String, input: Input[E]) extends Exception(s"Iteratee error '$msg' on input $input")

object IterateeObserver {
  def apply[In, A](iteratee: Iteratee[In, A], inMap: In => Input[In] = Input.El(_: In))(implicit ec: ExecutionContext): Future[IterateeObserver[In, A]] = {
    implicit val pec = ec.prepare()
    iteratee.unflatten.map(step => new IterateeObserver(step, inMap)(pec))(pec)
  }
}

// FIXME: Variance
class IterateeObserver[In, A](
    step: Step[In, A],
    inMap: In => Input[In])(
    implicit ec: /* prepared */ ExecutionContext
    ) extends ExclusiveObserver[In] {
  protected def queueProcessingContext = ec

  // I think var is OK as ExclusiveObserver guarantees memory ordering
  private var currentStep: Step[In, A] = step

  private val resultPromise = Promise[A]()

  def result: Future[A] = resultPromise.future

  private def setResultFromStep(step: Step[In, A]) {
    step match {
      case Step.Done(a, _) =>
        resultPromise.success(a)
      case Step.Error(msg, in) =>
        resultPromise.failure(new IterateeErrorException(msg, in))
      case _ =>
        ()
    }
  }

  val subs = new ClosableSubscriptions[In](step.isInstanceOf[Step.Cont[_, _]])
  def addInput(sub: SubscriptionOutput[In]) = subs.addInput(sub)
  def removeInput(sub: SubscriptionOutput[In]) = subs.removeInput(sub)

  setResultFromStep(currentStep)

  protected def exclusiveApply(in: In, sub: SubscriptionOutput[In]): Future[Unit] = {
    currentStep match {
      case Step.Cont(_) =>
        feedInput(in, sub)
      case _ =>
        // Should never be called...
        sub.close(/* cannot process given input */)
        Future.successful(())
    }
  }

  private def feedInput(in: In, sub: SubscriptionOutput[In]): Future[Unit] = {
    def updateStepAndMaybeClose(step: Step[In, A]): Boolean = {
      val doClose = !step.isInstanceOf[Step.Cont[_, _]]
      if (doClose) subs.close()
      setResultFromStep(step)
      currentStep = step
      doClose
    }

    // FIXME: Handle errors executing inMap
    currentStep.it.feed(inMap(in)).flatMap { fedIteratee =>
        fedIteratee.unflatten.map { fedStep =>
          val subClosed = updateStepAndMaybeClose(fedStep)
          if (!subClosed) sub.resume()
          ()
        }
    } recover {
      case _: Throwable =>
        updateStepAndMaybeClose(Step.Error[In]("Unexpected error processing input: aborting processing", Input.Empty))
        ()
    }
  }
}
