import akka.actor._
import akka.pattern.ask
import akka.util.Timeout

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.{Failure, Success}

trait Wrapper[A] {
  def calc: A
}

class CombineActor[A] extends Actor {
  override def receive = {
    case w:Wrapper[A] => { println("x"); w.calc }
    case _ => println("AS?")
  }
}



implicit val timeout:Timeout = Timeout(12 seconds)
import scala.concurrent.ExecutionContext.Implicits.global

val system = ActorSystem("HelloSystem")
  // default Actor constructor
val combineActor = system.actorOf(Props(new CombineActor[Int]), name = "combineactor")
val f = ask(combineActor, new Wrapper[Int] {
  def calc: Int = 2
}).mapTo[Int]


/*f.onComplete {
  case Success(s) => println(s"S= $s")
  case Failure(f) => println(s"F= $f")
}*/
Await.result(f, 5 seconds)



Console.println("Bye!")



