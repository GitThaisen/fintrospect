package io.fintrospect.parameters

import com.twitter.finagle.http.Message

/**
  * Parameters which are bound to request/response headers
  */
object Header {

  trait Mandatory[T] extends io.fintrospect.parameters.Mandatory[T, Message]
  with MandatoryRebind[T, Message, RequestBinding] {
    self: Bindable[T, RequestBinding] =>
  }

  trait MandatorySeq[T] extends io.fintrospect.parameters.Mandatory[Seq[T], Message] with MandatoryRebind[Seq[T], Message, RequestBinding] {
    self: Bindable[Seq[T], RequestBinding] =>
  }

  trait Optional[T] extends io.fintrospect.parameters.Optional[T, Message]
  with OptionalBindable[T, RequestBinding]
  with OptionalRebind[T, Message, RequestBinding] {
    self: Bindable[T, RequestBinding] =>
  }

  trait OptionalSeq[T] extends io.fintrospect.parameters.Optional[Seq[T], Message]
  with OptionalBindable[Seq[T], RequestBinding]
  with OptionalRebind[Seq[T], Message, RequestBinding] {
    self: Bindable[Seq[T], RequestBinding] =>
  }

  val required = new Parameters[HeaderParameter, Mandatory] with MultiParameters[MultiHeaderParameter, MandatorySeq] {
    override def apply[T](spec: ParameterSpec[T]) = new SingleHeaderParameter[T](spec) with Mandatory[T] {
      override def <--?(message: Message) = get(message, identity)
    }

    override val multi = new Parameters[MultiHeaderParameter, MandatorySeq] {
      override def apply[T](spec: ParameterSpec[T]) = new MultiHeaderParameter[T](spec) with MandatorySeq[T] {
        override def <--?(message: Message) = get(message, identity)
      }
    }
  }

  val optional = new Parameters[HeaderParameter, Optional] with MultiParameters[MultiHeaderParameter, OptionalSeq] {
    override def apply[T](spec: ParameterSpec[T]) = new SingleHeaderParameter[T](spec) with Optional[T] {
      override def <--?(message: Message) = get(message, Some(_))
    }

    override val multi = new Parameters[MultiHeaderParameter, OptionalSeq] {
      override def apply[T](spec: ParameterSpec[T]) = new MultiHeaderParameter[T](spec) with OptionalSeq[T] {
        override def <--?(message: Message) = get(message, Some(_))
      }
    }

  }
}
