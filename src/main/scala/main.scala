import sys.process._
import java.util.concurrent.atomic._
import java.util.concurrent.atomic.AtomicLong
import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent._
import scala.util.Random
import java.io.File

// object TimeChecker {
//   var data: scala.collection.mutable.HashMap[String, Long] = scala.collection.mutable.HashMap()
//   def runWithClock[A](name: String)(block: => A): A = {
//     if(true) {
//       val t0 = System.currentTimeMillis()
//       val result = block
//       val t1 = System.currentTimeMillis()
//       val old: Long = data.getOrElse(name, 0)
//       data.update(name, old + t1 - t0)
//       // println(s"Elapsed time in ${name} : " + (t1 - t0))
//       result
//     }
//     else {
//       block
//     }
//   }
// }

object CommonLogics {
  val CSI = """\e["""

  def exec(cmd: String): (Int, String) = {
    val x = new StringBuilder
    //merge stdout and stderr
    val myProcessLogger =
      ProcessLogger(x.append(_).append("\n"), x.append(_).append("\n"))
      // o => x.append(o + "\n"),
      // e => x.append(e + "\n"))
    // val retCode = stringToProcess("/bin/sh -c" + cmd) ! myProcessLogger // this does not work..
    //http://alvinalexander.com/scala/how-to-handle-wildcard-characters-running-external-commands
    val retCode = stringSeqToProcess(Seq("/bin/sh", "-c", cmd)) ! myProcessLogger
    (retCode, x.result())
 }
}

import CommonLogics._

object LLVMBerryLogics {
  type t = scala.collection.immutable.List[_]
  val SIMPLBERRY_DIR = "/home/youngju.song/myopt/simplberry_8.5"
  val OUT_NAME = "output"
  val NOT_PLAINS: List[String] = s"src.ll tgt.ll $OUT_NAME.ll".split(" ").toList
  val OPT_OPTION = "-O2"

  def get_ll_bases(dir_name: String) = {
    val ret = exec(s"ls ${dir_name}/**/*.ll")._2.split("\n").filterNot{x =>
      val y = x.split('.')
      // MUST split with '.', not "."
      // or it is matched to wild card or something
      y.size >= 2 && NOT_PLAINS.contains(y.takeRight(2).mkString("."))}.
      map(remove_one_extension(_))
    Random.shuffle(ret.toList)
    // def recursive_list_files(f: File): Array[File] = {
    //   val these = f.listFiles
    //   these ++ these.filter(_.isDirectory).flatMap(recursive_list_files)
    // }

    // for(f <- recursive_list_files(new File(dir_name))
    //   if (f.isFile && {
    //     val x = f.getName().split('.')
    //     (x.lastOption == Some("ll")) &&
    //     (x.size >= 2 && !NOT_PLAINS.contains(x.takeRight(2).mkString(".")))
    //   })
    // ) yield f.getAbsolutePath().split('.').dropRight(1).mkString(".")
  }

  def remove_one_extension(x: String): String =
    x.split('.').dropRight(1).mkString(".")

  def get_triple_bases(ll_base: String) = {
    // val base = remove_one_extension(ll_base)
    // println(base)
    // println(ll_base)
    exec(s"ls ${ll_base}.*.*.src.bc")
  }

  // #foo -> foo.func.0, foo.func.1, foo.func.2 ...
  // def get_tri_bases(base)
  // Dir["#{base}.*.*.src.bc"].map{|i| i.split(".")[0...-2].join(".")}
  // end

  def make = ???

  def generate(ll_base: String) = {
    // cmd = "opt -stats #{OPT_OPTION} #{base}.ll -o #{base}.#{OUT_NAME}.ll -S 2>&1"
    // result = %x(zsh -c "#{cmd}")
    // x = [$?.success?? :generate_success : :generate_fail, cmd, result]
    // File.open("#{base}.result", 'w').write(result) #unless(x[0] == :generate_success)
    val cmd = s"opt -stats ${OPT_OPTION} ${ll_base}.ll -o ${ll_base}.${OUT_NAME}.ll -S"
    exec(cmd)
  }

  def validate(triple_base: String) = {
    // result = %x(zsh -c "../ocaml_refact/main.native #{src} #{tgt} #{hint} 2>&1")
    val hint = triple_base + ".hint.json"
    val src = triple_base + ".src.bc"
    val tgt = triple_base + ".tgt.bc"
    val cmd = s"${SIMPLBERRY_DIR}/../ocaml_refact/main.native #{src} #{tgt} #{hint}"
    exec(cmd)
  }

  def which_opt = ???

  // def compile
  // cur_dir = run("pwd").chop
  // Dir.chdir("#{SIMPLBERRY_DIR}/.build/llvm-obj")
  // run("cmake --build . -- opt -j24")
  // Dir.chdir("#{SIMPLBERRY_DIR}/")
  // run("make refact -j24")
  // Dir.chdir("#{cur_dir}")
  // end

  // def which_opt(tri_base)
  // result = JSON.parse(File.read(tri_base + ".hint.json"))["opt_name"]
  // if result.nil? then
  // raise "opt_name is nil. parse result = #{result}"
  // end
  // result
  // end
  // end
  // end
}

object MainScript extends App {
  val GQ = new ConcurrentLinkedQueue[String]
  val VQ = new ConcurrentLinkedQueue[String]
  // var GQ_processed = new AtomicInteger(0)
  // var GQ_processed = 0

  // val GQR = new AtomicReference(List[JobResult]())
  // val VQR = new AtomicReference(List[JobResult]())
  var GQR = scala.collection.mutable.Queue[JobResult]()
  var VQR = scala.collection.mutable.Queue[JobResult]()
  // listbuffer? arraybuffer?

  // val GQR = new ConcurrentLinkedQueue[WorkResult]()
  // val VQR = new ConcurrentLinkedQueue[WorkResult]()

  LLVMBerryLogics.get_ll_bases(
    "/home/youngju.song/myopt/simplberry_8.5" +
      "/simplberry-tests" +
      "/llvm_regression_tests"
      // "/inputs_full"
  ).foreach(GQ.offer(_))
  println(GQ.size)
  val GQ_total = GQ.size
  var VQ_current_total = 0

  sealed abstract class Job
  case class GQJob(val ll_base: String) extends Job
  case class VQJob(val triple_base: String) extends Job
  case object Nothing extends Job
  case object Terminate extends Job
  // just Option Boolean?

  class JobResult (
    val FileSize: Long,
    val Time: Double,
    val Generated: Option[Int]
  ) {} //without val, it is private

  var count = 0

  def fetchNextJob: Job = {
    // MainScript.synchronized {
    val GQ_total_time_elapsed =
      GQR.foldLeft(0.0)((s, i) => s + i.Time)
    val VQ_total_time_elapsed =
      VQR.foldLeft(0.0)((s, i) => s + i.Time)

    var VQ_estimated_total: Double = {
      val num_generated = GQR.foldLeft(0)((s, i) => s + i.Generated.get)
      1.0 * num_generated * GQ_total / GQR.size
    }

    val GQ_estimated_single_time = GQ_total_time_elapsed / GQR.size

    val VQ_estimated_single_time = VQ_total_time_elapsed / VQR.size

    val GQ_estimated_ETA = GQ_estimated_single_time * (GQ_total - GQR.size)
    // val GQ_estimated_ETA = GQ_estimated_single_time * GQ.size

    val VQ_estimated_ETA = VQ_estimated_single_time * (VQ_estimated_total - VQR.size)

    // }

    count += 1
    if(count % 50 == 0) {
      // println(GQ_estimated_ETA + " " + VQ_estimated_ETA)
      // println(
      //   GQR.foldLeft(0)((s, i) => s + i.Generated.get) + " " +
      //     GQ_total + " " +
      //     GQR.size
      // )
      // print(s"${CSI}1A")
      // print("\r")
      System.out.print("\33[1A\33[2K");
      // print("#{CSI}1A")
      // print("\r")
      // print("#{CSI}1A")
      // print("\r")

      println("####" + VQ_current_total + " " + VQ_estimated_total)


      // println(GQ_total_time_elapsed + " " + VQ_total_time_elapsed + " " +
      //   VQ_estimated_total)

      // println("GQ : " + GQ.size + ", GQR : " + GQR.size + "/" + GQ_total)
      // println("VQ : " + VQ.size + ", VQR : " + VQR.size + "/" + VQ_total)
    }
    if(GQR.size == GQ_total && VQR.size == VQ_current_total) return Terminate


    def tryGQ: Job = {
      val bb = Option(GQ.poll)
      if(bb.isDefined) GQJob(bb.get)
      else Nothing
    }

    def tryVQ: Job = {
      val bb = Option(VQ.poll)
      if(bb.isDefined) VQJob(bb.get)
      else Nothing
    }

    var ret: Job =
      if(GQ_estimated_ETA > VQ_estimated_ETA)
        tryGQ
      else
        tryVQ

    if(ret == Nothing) ret = tryGQ
    if(ret == Nothing) ret = tryVQ
    ret
    // ret = {
    //   if(ret == Nothing) tryGQ
    //   else ret
    // }
    // if(ret == Nothing)
    //   ret = tryVQ
    // else {
    //   val ret2 = Option(VQ.poll)
    //   if(ret2.isDefined) VQJob(ret2.get)
    //   else Nothing
    // }
  }

  def processGQ(ll_base: String): JobResult = {
    val t0 = System.currentTimeMillis
    val FileSize = (new File(ll_base + ".ll")).length
    try {
      val res = LLVMBerryLogics.generate(ll_base)
      val tri_bases = LLVMBerryLogics.get_triple_bases(ll_base)
      if(tri_bases._1 == 0) {
        for(tri_base <- tri_bases._2.split("\n")) {
          VQ.offer(tri_base)
        }
        MainScript.synchronized { VQ_current_total += tri_bases._2.split("\n").size }
      }
      val t1 = System.currentTimeMillis
      val gens = if(tri_bases._1 == 0) Some(tri_bases._2.split("\n").size) else Some(0)
      // println(tri_bases._2.split("\n"))
      // println("GENS -------------" + gens)
      new JobResult(FileSize, (t1 - t0)/1000.0, gens)
    }
    catch {
      case e: Throwable =>
        println("EEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE " + e)
        val t1 = System.currentTimeMillis
        new JobResult(FileSize, (t1 - t0)/1000.0, None)
    }
  }

  def processVQ(triple_base: String): JobResult = {
    val t0 = System.currentTimeMillis
    val FileSize = (new File(triple_base + ".ll")).length
    val res = LLVMBerryLogics.validate(triple_base)
    // println(res._1)
    val t1 = System.currentTimeMillis
    new JobResult(FileSize, (t1 - t0)/1000.0, None)
  }

  class MyThread extends Thread {
    override def run {
      def runner: Unit = {
        fetchNextJob match {
          case GQJob(ll_base) =>
            val res = processGQ(ll_base)
            MainScript.synchronized { GQR += res }
            runner
          case VQJob(triple_base) =>
            val res = processVQ(triple_base)
            MainScript.synchronized { VQR += res }
            runner
          case Nothing => Thread.sleep(5000) ; runner
          case Terminate => ()
        }
      }
      runner
    }
  }

  val threads: IndexedSeq[Thread] =
    for (i <- 1 to 24) yield {
      val thread = new MyThread 
      thread.start
      thread
    }
  threads.foreach(_.join)
  // GQR.foreach(i => println(i.FileSize + "\t" + i.Time + "\t" + i.Generated.get))
  assert(GQ.size == 0 && GQR.size == GQ_total)
  assert(VQ.size == 0 && VQR.size == VQ_current_total)
}

// object Runner {
//   private val x = new AtomicReference(List[String]())
//   val GQ = new ConcurrentLinkedQueue[Int]()
//   val VQ = new ConcurrentLinkedQueue[Int]()
//   type t = List[_]
//   type l = java.util.Queue[_]
//   var a = 0

//   for (i <- 1 to 10) GQ.offer(i)
//   println(GQ)
//   val threads: IndexedSeq[Thread] =
//     for (i <- 1 to 24) yield {
//       val thread = new Thread {
//         override def run {

//           import ProcessBuilder._
//           val tmp = stringToProcess("ls -al").!
//           val ttt = (stringToProcess("ls -al")).!!
//           println(ttt)


//           // val result: Process = Process("ls -al")
//           // println(result)
//           val z = Option(GQ.poll)
//           if(z.isDefined) VQ.offer(z.get)
//           val n = Random.nextInt() / 100
//           a = a + n
//           // println(z)
//           println(GQ.size + " " + VQ.size)
//           // println(GQ)
//           Thread.sleep(500)
//           a = a - n
//         }
//       }
//       thread.start
//       thread
//       // Thread.sleep(50) // slow the loop down a bit
//     }
//   threads.foreach(_.join)
//   println(a)
// }

MainScript.main(args)
