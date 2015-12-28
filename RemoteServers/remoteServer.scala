import akka.actor._
import java.io.File
import com.typesafe.config.ConfigFactory
import java.security.MessageDigest
import scala.util.Random
import akka.routing.RoundRobinPool
import akka.actor.PoisonPill

case class initMining()
case class Result(key: String,hash:String)
case class findTarget()
case class ContactServer()
case class startMining(target: Int,inputRange: Int)
case class remoteMining()

object remoteServer extends App  {
  override def main(args: Array[String]) {
    val target = args(0).toInt
    val configFile = getClass.getClassLoader.getResource("remoteApplication.conf").getFile()
    val config = ConfigFactory.parseFile(new File(configFile))
    val system = ActorSystem("HelloRemoteSystem",config)
    val remoteActor = system.actorOf(Props(new RemoteActor(target)), name = "RemoteActor")
    remoteActor ! initMining()
  }
  
}

class RemoteActor(target: Int) extends Actor {
  val num_workers = 8
  val input_max = 40000000
  val range = input_max/num_workers
  var assignments = 0
  var returns = 0
  val start : Long = System.currentTimeMillis
  val workerRouter = context.actorOf(Props(new Worker(target,range))
      .withRouter(RoundRobinPool(num_workers)),
       name = "workerRouter"
  )
  
  def receive = {
    case "start" =>
       // sender ! startMining(target,range)
      sender ! target
    case msg: String => 
//        println(s"\n---------got message $msg --------------")
        val key = msg.split("::")(0)
        val hash = msg.split("::")(1)
//        println(s"RemoteActor received message '$msg'")
        println("\n%s\t%s".format(key,hash))
        
    case initMining() =>
        println("Starting mining...")
        for(i <- 1 to num_workers) {
          workerRouter ! findTarget()
        }
        
    case Result(key,hash) =>      //receive result
       println("\n%s\t%s".format(key,hash))
       //println("Shutting down.....")
  }
}

class Worker(target : Int, range: Int) extends Actor{
  private val md = MessageDigest.getInstance("SHA-256")
  var round = 0;
  var pattern = new StringBuilder()
  for(i<- 1 to target)
    pattern.append('0')
  var target_str = pattern.toString()
  def receive = {
    case findTarget() =>
      findBitCoin(target)
  }
  
  def findBitCoin(target:Int) {
    //println(s"Am here for round:$round")
	val max = 50
	var tmp = 0
    for(i <- 1 to range) {
      val key = getRandomKey()
      val hash = sha256(key)
      if(hash.startsWith(target_str)) {
        sender ! Result(key,hash)
		tmp += 1
		if(tmp > max)
			return
      }
    }
  }
  
  def sha256(s:String) : String = {
    var bytes = md.digest(s.getBytes("UTF-8")).toList
    bytes.map{ b => String.format("%02x", java.lang.Byte.valueOf(b))}.mkString("")
  }
 
  def getRandomKey(): String = {
    val len = Random.nextInt(15)
    val chars = ('a' to 'z') ++ ('A' to 'Z') ++ ('0' to '9')
    "chennakesava+siva92;"+randomStringFromCharList(len,chars)
  }
  
  def randomStringFromCharList(length: Int, chars: Seq[Char]): String = {
    val sb = new StringBuilder
    for (i <- 1 to length) {
      val randomNum = util.Random.nextInt(chars.length)
      sb.append(chars(randomNum))
    }
    sb.toString
  }
}
