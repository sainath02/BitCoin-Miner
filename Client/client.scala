package remotelocal
import akka.actor._
import scala.util.Random
import java.io.File
import com.typesafe.config.ConfigFactory
import java.security.MessageDigest
import akka.routing.RoundRobinPool
import akka.actor.PoisonPill

/**
 * @author SIVA
 */
case class initMining()
case class Result(key: String,hash:String)
case class FindTarget()
case class startMining()

object client extends App {
  override def main(args: Array[String]) {
    val configFile = getClass.getClassLoader.getResource("localApplication.conf").getFile()
    val config = ConfigFactory.parseFile(new File(configFile))
    implicit val system = ActorSystem("LocalSystem",config)
    val ipAddress = args(0)
    val localActor = system.actorOf(Props(new LocalActor(ipAddress)), name = "LocalActor")  // the local actor
    localActor ! startMining()
  }
}

class LocalActor(ipAddress : String) extends Actor {
  // create the remote actor
  val remote = context.actorFor("akka.tcp://HelloRemoteSystem@"+ipAddress+":2552/user/RemoteActor")
  var counter = 0
  val num_workers = 8
  val inputRange = 5000000
  def receive = {
    case startMining() =>
      remote ! "start"
    case msg: Int =>
        for(i <- 1 to num_workers) 
           context.actorOf(Props(new Worker(msg.toInt,inputRange))) ! FindTarget()
    case Result(key,hash) => 
         remote ! key+"::"+hash
    case "Die" =>
         context.stop(self)
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
    case FindTarget() =>
      findBitCoin(target)
  }
  
  def findBitCoin(target:Int) {
    round += 1
    val max = 50
    var count = 0
    for(i <- 1 to range) {
      val key = getRandomKey()
      val hash = sha256(key)
      if(hash.startsWith(target_str)) {
		count += 1
        sender ! Result(key,hash)
		if(count > max)
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
    "chennakesava;"+randomStringFromCharList(len,chars)
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
