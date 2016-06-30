import sys.process._
import java.util.concurrent.atomic._
import java.util.concurrent.atomic.AtomicLong
import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent._
import scala.util.Random
import java.io.File
import scala.util.parsing.json._
import collection.mutable.Map

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
    val myProcessLogger = {
      def appendLine(s: String) = {
        // try {
          x.append(s + "\n")
        // }
        // catch {
        //   case _: Throwable =>
        //   println; println; println; println(cmd); println; println; println; println; println; println
        // }
      }
      ProcessLogger(appendLine, appendLine)
    }
    // val retCode = stringToProcess("/bin/sh -c" + cmd) ! myProcessLogger // this does not work..
    //http://alvinalexander.com/scala/how-to-handle-wildcard-characters-running-external-commands

    val retCode =
      // try {
      stringSeqToProcess(Seq("/bin/sh", "-c", cmd)) ! myProcessLogger
    // }
    // catch {
    //   case _: Throwable =>
    //     println; println; println; println(cmd); println; println; println; println; println; println
    //     println(cmd)
    //     println(cmd)
    //     println(cmd)
    //     println(cmd)
    //     println(cmd)
    //     println(cmd)
    //     println; println; println; println(cmd); println; println; println; println; println; println
    //     println; println; println; println(cmd); println; println; println; println; println; println
    //     println; println; println; println(cmd); println; println; println; println; println; println
    //     System.exit(2)
    //     1
    // }
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

  def get_ll_bases(dir_name: String): List[String] = {
    val ret = exec(s"ls ${dir_name}/**/*.ll")._2.split("\n").filterNot{x =>
      val y = x.split('.')
      // MUST split with '.', not "."
      // or it is matched to wild card or something
      y.size >= 2 && NOT_PLAINS.contains(y.takeRight(2).mkString("."))}.
      map(remove_extensions(1))
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

  def remove_extensions(n: Int)(x: String): String =
    x.split('.').dropRight(n).mkString(".")

  def get_triple_bases(ll_base: String): List[String] = {
    val t = exec(s"ls ${ll_base}.*.*.src.bc")
    if(t._1 == 0) t._2.split("\n").map(remove_extensions(2)).toList
    else List()
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
    val cmd = s"${SIMPLBERRY_DIR}/ocaml_refact/main.native ${src} ${tgt} ${hint}"
    exec(cmd)
  }

  def parseGenerateResult(x: (Int, String)): String = {
    if(x._1 == 0) "generate_success"
    else "generate_fail"
  }

  def parseValidateResult(x: (Int, String)): String = {
    def f(y: String) = x._2.contains(y)

    // println(x._2)
    if(f("Validation failed.")) "validation fail"
    else if(f("Validation succeeded.")) "validation succeed"
    else "validation unknown"
  }
  // def classify_result(result)
  //   if ($?.success?) && result["Validation failed."]
  //   then
  //     raise "process succeeded, but validation says it failed"
  //   end

  //   return :validation_admitted if result["Validation Admitted."]
  //   return :assertion_failed if result["Assertion failed."]
  //   return :validation_not_supported if (result["Not_Supported"] or result["not supported"])
  //   return :validation_failed if result["Validation failed."]
  //   return :validation_success if result["Validation succeeded."]
  //   return :validation_unknown
  // end


  def whichOpt(triple_base: String): String = {
    val hint = scala.io.Source.fromFile(triple_base + ".hint.json").mkString
    val json = JSON.parseRaw(hint).get.asInstanceOf[JSONObject].obj
    json.get("opt_name").get.asInstanceOf[String]
    //exception handling?
  }


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

  var GQR = scala.collection.mutable.Queue[GQJobResult]()
  var VQR = scala.collection.mutable.Queue[VQJobResult]()

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

  abstract class JobResult {
    val fileSize: Long
    val time: Double
    val exitResult: String
  } //without val, it is private

  class GQJobResult(
    val fileSize: Long,
    val time: Double,
    val generated: Int,
    val exitResult: String
  ) extends JobResult

  class VQJobResult(
    val fileSize: Long,
    val time: Double,
    val optName: String,
    val exitResult: String
  ) extends JobResult

  var count = 0

  def fetchNextJob: Job = {
    val GQ_total_time_elapsed =
      GQR.foldLeft(0.0)((s, i) => s + i.time)
    val VQ_total_time_elapsed =
      VQR.foldLeft(0.0)((s, i) => s + i.time)

    var VQ_estimated_total: Double = {
      val num_generated = GQR.foldLeft(0)((s, i) => s + i.generated)
      1.0 * num_generated * GQ_total / GQR.size
    }

    val GQ_estimated_single_time = GQ_total_time_elapsed / GQR.size

    val VQ_estimated_single_time = VQ_total_time_elapsed / VQR.size

    val GQ_estimated_ETA = GQ_estimated_single_time * (GQ_total - GQR.size)
    // val GQ_estimated_ETA = GQ_estimated_single_time * GQ.size

    val VQ_estimated_ETA = VQ_estimated_single_time * (VQ_estimated_total - VQR.size)

    count += 1
    if(count % 50 == 0) {
      // println(GQ_estimated_ETA + " " + VQ_estimated_ETA)
      // println(
      //   GQR.foldLeft(0)((s, i) => s + i.generated.get) + " " +
      //     GQ_total + " " +
      //     GQR.size
      // )
      // print(s"${CSI}1A")
      // print("\r")
      System.out.print("\33[1A\33[2K");
      System.out.print("\33[1A\33[2K");
      System.out.print("\33[1A\33[2K");
      // print("#{CSI}1A")
      // print("\r")
      // print("#{CSI}1A")
      // print("\r")

      println(GQR.size + "/" + GQ_total)
      println(VQR.size + "/" + VQ_estimated_total)
      println("####" + VQ_current_total + " " + VQ_estimated_total)


      // println(GQ_total_time_elapsed + " " + VQ_total_time_elapsed + " " +
      //   VQ_estimated_total)

      // println("GQ : " + GQ.size + ", GQR : " + GQR.size + "/" + GQ_total)
      // println("VQ : " + VQ.size + ", VQR : " + VQR.size + "/" + VQ_total)
    }
    if(GQR.size == GQ_total && VQR.size == VQ_current_total) Terminate
    else {
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
    }
  }

  def processGQ(ll_base: String): GQJobResult = {
    val t0 = System.currentTimeMillis
    val fileSize = (new File(ll_base + ".ll")).length
    val res = LLVMBerryLogics.generate(ll_base)
    val tri_bases = LLVMBerryLogics.get_triple_bases(ll_base)
    tri_bases.foreach(VQ.offer(_))
    MainScript.synchronized { VQ_current_total += tri_bases.size }
    val t1 = System.currentTimeMillis
    val exitRes = LLVMBerryLogics.parseGenerateResult(res)
    new GQJobResult(fileSize, (t1 - t0)/1000.0, tri_bases.size, exitRes)
  }

  def processVQ(triple_base: String): VQJobResult = {
    val t0 = System.currentTimeMillis
    val fileSize = (new File(triple_base + ".ll")).length
    val res = LLVMBerryLogics.validate(triple_base)
    val optName = LLVMBerryLogics.whichOpt(triple_base)
    val t1 = System.currentTimeMillis
    val exitRes = LLVMBerryLogics.parseValidateResult(res)
    new VQJobResult(fileSize, (t1 - t0)/1000.0, optName, exitRes)
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
  // GQR.foreach(i => println(i.fileSize + "\t" + i.time + "\t" + i.generated.get))
  println(GQ_total + " " + VQ_current_total)
  var table: Map[String, Map[String, Int]] =
    Map()
    // Map[String, Map[String, Int]]().withDefaultValue(
    //   // new scala.collection.mutable.HashMap[String, Int]()
    //   Map[String, Int]().withDefaultValue(0)
    // )
    // new Map[String, Map[String, Int]]()
    //   { override def default(key: String) = Map() }
  VQR.foreach{i =>
    // table(i.optName)(i.exitResult) += 1
    var t = table.getOrElse(i.optName, Map())
    val tt = t.getOrElse(i.exitResult, 0) + 1
    table.update(i.optName, t.updated(i.exitResult, tt))
  }
  // PAR MAP?
  // table("A")("B") = 2
  println(table.size)
  println(table.keys)
  println(table)
  table.foreach(println(_))
  // VQR.par.foreach(i => table.updated(i.optName, )
  assert(GQ.size == 0 && GQR.size == GQ_total)
  assert(VQ.size == 0 && VQR.size == VQ_current_total)
}

println
println
println
MainScript.main(args)
