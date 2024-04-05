package module1

object opt {

  sealed trait Option[+T] {
    def isEmpty: Boolean

    def get: T

    def map[B](f: T => B): Option[B]

    def flatMap[B](f: T => Option[B]): Option[B]

    def printIfAny(): Unit

    def zip[B](other: Option[B]): Option[(T, B)]

    def filter(p: T => Boolean): Option[T]
  }

  case class Some[V](v: V) extends Option[V] {
    override def isEmpty: Boolean = false

    override def get: V = v

    override def map[B](f: V => B): Option[B] = Some(f(v))

    override def flatMap[B](f: V => Option[B]): Option[B] = f(v)

    override def printIfAny(): Unit = println(v)

    override def zip[B](other: Option[B]): Option[(V, B)] = other match {
      case Some(b) => Some((v, b))
      case None => None
    }

    override def filter(p: V => Boolean): Option[V] = if (p(v)) this else None
  }

  case object None extends Option[Nothing] {
    override def isEmpty: Boolean = true

    override def get: Nothing = throw new NoSuchElementException("get on empty option")

    override def map[B](f: Nothing => B): Option[B] = None

    override def flatMap[B](f: Nothing => Option[B]): Option[B] = None

    override def printIfAny(): Unit = ()

    override def zip[B](other: Option[B]): Option[(Nothing, B)] = None

    override def filter(p: Nothing => Boolean): Option[Nothing] = None
  }

  object Option {
    def apply[T](v: T): Option[T] =
      if (v == null) None
      else Some(v)
  }
}

object list {
  sealed trait List[+T] {
    def ::[A >: T](head: A): List[A]
    def :::[A >: T](prefix: List[A]): List[A]
    def map[B](f: T => B): List[B]
    def flatMap[B](f: T => List[B]): List[B]
    def filter(p: T => Boolean): List[T]
    def reverse: List[T]
    def mkString(separator: String): String
    def incList: List[Int]
    def shoutString[B >: T]: List[String]
    def printArgs[A](args: A*): Unit
  }

  case object Nil extends List[Nothing] {
    override def ::[A](head: A): List[A] = Cons(head, Nil)
    override def :::[A](prefix: List[A]): List[A] = prefix
    override def map[B](f: Nothing => B): List[B] = Nil
    override def flatMap[B](f: Nothing => List[B]): List[B] = Nil
    override def filter(p: Nothing => Boolean): List[Nothing] = Nil
    override def reverse: List[Nothing] = Nil
    override def mkString(separator: String): String = ""
    override def incList: List[Int] = Nil
    override def shoutString[B >: Nothing]: List[String] = Nil

    override def printArgs[Nothing](args: Nothing*): Unit = Nil
  }

  case class Cons[T](head: T, tail: List[T]) extends List[T] {
    override def ::[A >: T](head: A): List[A] = Cons(head, this)
    override def :::[A >: T](prefix: List[A]): List[A] = prefix match {
      case Nil => this
      case Cons(h, t) => h :: (t ::: this)
    }

    override def map[B](f: T => B): List[B] = Cons(f(head), tail.map(f))
    override def flatMap[B](f: T => List[B]): List[B] = f(head) match {
      case Nil => tail.flatMap(f)
      case Cons(h, t) => Cons(h, t ::: tail.flatMap(f))
    }
    override def filter(p: T => Boolean): List[T] = if (p(head)) Cons(head, tail.filter(p)) else tail.filter(p)
    override def reverse: List[T] = tail.reverse ::: Cons(head, Nil)
    def mkString(separator: String): String = tail match {
      case Nil => head.toString
      case _ => head.toString + separator + tail.mkString(separator)
    }
    def incList: List[Int] = Cons(head.asInstanceOf[Int] + 1, tail.incList)
    def shoutString[B >: T]: List[String] = this.map(s => "!" + s)
    override def printArgs[A](args: A*): Unit = args.foreach(print(_))
  }

  object List {


    def apply[T](elems: T*): List[T] = {
      if (elems.isEmpty) Nil
      else Cons(elems.head, apply(elems.tail: _*))
    }
  }
}