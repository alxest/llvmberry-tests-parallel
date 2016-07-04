import sys.process._
import java.util.concurrent.atomic._
import java.util.concurrent.atomic.AtomicLong
import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent._
import scala.util.Random
import java.io.File
import scala.util.parsing.json._

object CommonLogics {
  def exec(cmd: String): (Int, String, String) = {
    val o = new StringBuilder
    val e = new StringBuilder
    val myProcessLogger =
      ProcessLogger(o.append(_).append("\n"), e.append(_).append("\n"))
    // val retCode = stringToProcess("/bin/sh -c" + cmd) ! myProcessLogger // this does not work..
    //http://alvinalexander.com/scala/how-to-handle-wildcard-characters-running-external-commands

    val retCode =
      stringSeqToProcess(Seq("/bin/sh", "-c", cmd)) ! myProcessLogger
    (retCode, o.result(), e.result())
  }

  def goPreviousLine =
    System.out.print("\u001b[1A\u001b[2K");
    // System.out.print("\33[1A\33[2K");

  def printBar(x: String = "") = {
    val width = 200
      // exec("tput cols")._2.trim.toInt - x.size
    val half_width = width/2
    println("-" * half_width + x + "-" * (width - half_width))
  }

  object TimeChecker {
      // new scala.collection.immutable.HashMap[String, Map[String, Int]]().
    var data: scala.collection.mutable.Map[String, Long] =
      new scala.collection.mutable.HashMap[String, Long]().withDefaultValue(0)
    val checkTime = true
    def runWithClock[A](name: String)(block: => A): A = {
      if(checkTime) {
        val t0 = System.currentTimeMillis()
        val result = block
        val t1 = System.currentTimeMillis()
        val old: Long = data.getOrElse(name, 0)
        data.update(name, old + t1 - t0)
        result
      }
      else block
    }
  }
}

import CommonLogics._

object LLVMBerryLogics {
  sealed class GResult
  case object GSuccess extends GResult
  case object GFail extends GResult

  sealed class VResult
  case object VSuccess extends VResult
  case object VFail extends VResult
  case object VAdmitted extends VResult
  case object VAssertionFail extends VResult
  case object VNotSupported extends VResult
  case object VUnknown extends VResult

  // val SIMPLBERRY_DIR = "/home/youngju.song/myopt/simplberry_8.5"
  val SIMPLBERRY_DIR = "/home/youngju.song/myopt/_simplberry"
  val opt_path = SIMPLBERRY_DIR + "/.build/llvm-obj/bin/opt"
  val main_native_path = s"${SIMPLBERRY_DIR}/ocaml_refact/main.native"
  val OUT_NAME = "output"
  val NOT_PLAINS: List[String] = s"src.ll tgt.ll $OUT_NAME.ll".split(" ").toList
  val OPT_OPTION = "-instcombine" //"-O2"
  val testDir = SIMPLBERRY_DIR + // "/home/youngju.song/myopt/simplberry_8.5" +
  "/simplberry-tests" +
  // "/programs_new"
  // "/llvm_regression_tests"
  "/speccpu2006-ll"

  def get_ll_bases(dir_name: String): List[String] = {
    val ret = exec(s"ls ${dir_name}/**/*.ll")._2.split("\n").filterNot{x =>
      // MUST split with '.', not "." or it is matched to wild card or something
      val y = x.split('.')
      y.size >= 2 && NOT_PLAINS.contains(y.takeRight(2).mkString("."))
    }.map(remove_extensions(1))
    Random.shuffle(ret.toList)
  }

  def remove_extensions(n: Int)(x: String): String =
    x.split('.').dropRight(n).mkString(".")

  def get_triple_bases(ll_base: String): List[String] = {
    val t = exec(s"ls ${ll_base}.*.*.src.bc")
    if(t._1 == 0) t._2.split("\n").map(remove_extensions(2)).toList
    else List()
  }

  def cleanByProducts = {
    exec(s"""cd ${testDir} && find . -name "*.src.bc" -delete""")
    exec(s"""cd ${testDir} && find . -name "*.tgt.bc" -delete""")
    exec(s"""cd ${testDir} && find . -name "*.src.ll" -delete""")
    exec(s"""cd ${testDir} && find . -name "*.tgt.ll" -delete""")
    exec(s"""cd ${testDir} && find . -name "*.output.ll" -delete""")
    exec(s"""cd ${testDir} && find . -name "*.result" -delete""")
  }

  def compile = {
    val generator_compile = exec(s"cd ${SIMPLBERRY_DIR}/ && make opt -j24")
    if(generator_compile._1 != 0) {
      println("Compile Failed!")
      println("stdout : " + generator_compile._2)
      println("stderr : " + generator_compile._3)
      assert(false)
    }
    val validator_compile = exec(s"cd ${SIMPLBERRY_DIR}/ && make refact -j24")
    if(validator_compile._1 != 0) {
      println("Compile Failed!")
      println("stdout : " + validator_compile._2)
      println("stderr : " + validator_compile._3)
      assert(false)
    }
  }

  def generate(ll_base: String) = {
    val cmd = s"${opt_path} ${OPT_OPTION} ${ll_base}.ll -o ${ll_base}.${OUT_NAME}.ll -S"
    exec(cmd)
  }

  def validate(triple_base: String) = {
    val hint = triple_base + ".hint.json"
    val src = triple_base + ".src.bc"
    val tgt = triple_base + ".tgt.bc"
    val cmd = s"${main_native_path} ${src} ${tgt} ${hint}"
    exec(cmd)
  }

  def classifyGenerateResult(x: (Int, String, String)): GResult = {
    if(x._1 == 0) GSuccess
    else GFail
  }

  def classifyValidateResult(x: (Int, String, String)): VResult = {
    def f(y: String) = x._3.split('\n').head.contains(y)

    if(f("Validation failed.")) VFail
    else if(f("Validation succeeded.")) VSuccess
    else if(f("Validation Admitted.")) VAdmitted
    else if(f("Assertion failed.")) VAssertionFail
    else if(f("Fatal error: exception Failure") &&
      (f("Not_Supported") || f("is not supported for now."))) VNotSupported
    // else if(f("llvm-obj/bindings/ocaml/llvm/llvm_ocaml.c:1388: llvm_instr_get_opcode: Assertion `o <= LLVMLandingPad' failed."))
    //   "Ocaml Binding Fail"
    else
      VUnknown
  }

  def getOptName(triple_base: String): String = {
    val hint = scala.io.Source.fromFile(triple_base + ".hint.json").mkString
    val json = JSON.parseRaw(hint).get.asInstanceOf[JSONObject].obj
    json.get("opt_name").get.asInstanceOf[String]
    //exception handling?
  }
}

object MainScript extends App {
  val GQ = new ConcurrentLinkedQueue[String]
  val VQ = new ConcurrentLinkedQueue[String]

  var GQR = scala.collection.mutable.Queue[GQJobResult]()
  var VQR = scala.collection.mutable.Queue[VQJobResult]()

  LLVMBerryLogics.get_ll_bases(LLVMBerryLogics.testDir).foreach(GQ.offer(_))
  println(GQ.size)
  val GQ_total = GQ.size
  var VQ_current_total = 0

  sealed abstract class Job
  case class GQJob(val ll_base: String) extends Job
  case class VQJob(val triple_base: String) extends Job
  case object Nothing extends Job
  case object Terminate extends Job
  // just Option Boolean?

  sealed abstract class JobResult {
    val fileSize: Long
    val time: Double
    // val classifiedResult: String
  } //without val, it is private

  class GQJobResult(
    val fileSize: Long,
    val time: Double,
    val generated: Int,
    val classifiedResult: LLVMBerryLogics.GResult
  ) extends JobResult

  class VQJobResult(
    val fileSize: Long,
    val time: Double,
    val optName: String,
    val classifiedResult: LLVMBerryLogics.VResult
  ) extends JobResult

  var count = 0

  def fetchNextJob: Job = {
    val GQ_total_time_elapsed =
      GQR.foldLeft(0.0)((s, i) => s + i.time)
    val VQ_total_time_elapsed =
      VQR.foldLeft(0.0)((s, i) => s + i.time)

    var VQ_estimated_total: Double = {
      val num_generated = GQR.foldLeft(0)((s, i) => s + i.generated)
      num_generated * Math.pow((1.0 * GQ_total / GQR.size), 1.3)
    }

    val GQ_estimated_single_time = GQ_total_time_elapsed / GQR.size

    val VQ_estimated_single_time = VQ_total_time_elapsed / VQR.size

    val GQ_estimated_ETA = GQ_estimated_single_time * (GQ_total - GQR.size)
    // val GQ_estimated_ETA = GQ_estimated_single_time * GQ.size

    val VQ_estimated_ETA = VQ_estimated_single_time * (VQ_estimated_total - VQR.size)

    count += 1
    if(count % 50 == 0) {
      MainScript.synchronized {
        TimeChecker.runWithClock("Print") {
          (1 to 6) foreach { _ => goPreviousLine }
          println(GQR.size + "/" + GQ_total)
          println(VQR.size + "/" + VQ_estimated_total)
          println("####" + VQ_current_total + " " + VQ_estimated_total)
          printGQR
          printVQRSimple
        }
        println(TimeChecker.data)
      }
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
    TimeChecker.runWithClock("GQ") {
      val t0 = System.currentTimeMillis
      val fileSize = (new File(ll_base + ".ll")).length
      val res = LLVMBerryLogics.generate(ll_base)
      val tri_bases = LLVMBerryLogics.get_triple_bases(ll_base)
      tri_bases.foreach(VQ.offer(_))
      MainScript.synchronized { VQ_current_total += tri_bases.size }
      val t1 = System.currentTimeMillis
      val exitRes = LLVMBerryLogics.classifyGenerateResult(res)
      if(exitRes != LLVMBerryLogics.GSuccess) {
        import java.nio.file.{Paths, Files}
        import java.nio.charset.StandardCharsets
        val result = "########## STDOUT\n" + res._2 +
        "\n\n\n ########## STDERR\n" + res._3

        Files.write(Paths.get(ll_base + ".result"), result.getBytes(StandardCharsets.UTF_8))
      }
      new GQJobResult(fileSize, (t1 - t0)/1000.0, tri_bases.size, exitRes)
    }
  }

  def processVQ(triple_base: String): VQJobResult = {
    TimeChecker.runWithClock("VQ") {
      val t0 = System.currentTimeMillis
      val fileSize = (new File(triple_base + ".ll")).length
      val res = LLVMBerryLogics.validate(triple_base)
      val optName = LLVMBerryLogics.getOptName(triple_base)
      val t1 = System.currentTimeMillis
      val exitRes = LLVMBerryLogics.classifyValidateResult(res)
      new VQJobResult(fileSize, (t1 - t0)/1000.0, optName, exitRes)
    }
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
          case Nothing => Thread.sleep(2000) ; runner
          case Terminate => ()
        }
      }
      runner
    }
  }

  println ; println ; printBar()
  println("Start Script")
  LLVMBerryLogics.compile
  println("Compile Done")
  LLVMBerryLogics.cleanByProducts
  println("cleanByProducts Done")
  for(i <- 1 to 12) println
  val threads: IndexedSeq[Thread] =
    for (i <- 1 to 24) yield {
      val thread = new MyThread 
      thread.start
      thread
    }
  threads.foreach(_.join)
  for(i <- 1 to 8) println
  println("Test Done")

  // GQR.foreach(i => println(i.fileSize + "\t" + i.time + "\t" + i.generated.get))
  println(GQ_total + " " + VQ_current_total)
  assert(GQ.size == 0 && GQR.size == GQ_total)
  assert(VQ.size == 0 && VQR.size == VQ_current_total)
  println ; println ; printBar()
  printGQR
  println ; println ; printBar()
  printVQR
  println ; println ; printBar()
  printVQRSimple
  println ; println ; printBar()

  def printRow[A](row_name: String)(table: Map[A, Int]) = {
    print(row_name.padTo(20, ' ') + " ---->   ")
    table.foreach(y => print((if(y._2 != 0) y.toString else "").padTo(20, ' ') + " "))
    println
  }
    // table.foreach{x =>
    //   println(x._1.padTo(20, ' ') + " ----> " + x._2)
    // }

  def printGQR = {
    val table: Map[LLVMBerryLogics.GResult, Int] =
      new scala.collection.immutable.HashMap[LLVMBerryLogics.GResult, Int]().
        withDefaultValue(0)
    val table_filled = GQR.foldLeft(table){(s, i) =>
      s.updated(i.classifiedResult, s(i.classifiedResult) + 1)
    }
    printRow("All Generation")(table_filled)
  }

  def printVQR = {
    val VblankRow = new scala.collection.immutable.HashMap[LLVMBerryLogics.VResult, Int]() +
    ((LLVMBerryLogics.VSuccess, 0)) +
    ((LLVMBerryLogics.VFail, 0)) +
    ((LLVMBerryLogics.VNotSupported, 0)) +
    ((LLVMBerryLogics.VAdmitted, 0)) +
    ((LLVMBerryLogics.VAssertionFail, 0)) +
    ((LLVMBerryLogics.VUnknown, 0))

    val table: Map[String, Map[LLVMBerryLogics.VResult, Int]] =
      new scala.collection.immutable.HashMap[String, Map[LLVMBerryLogics.VResult, Int]]().
        withDefaultValue(
          // new scala.collection.immutable.HashMap[String, Int]().
          //   withDefaultValue(0)
          VblankRow
        )


    val table_filled = VQR.foldLeft(table){(s, i) =>
      val trans = s(i.optName).updated(
        i.classifiedResult,
        s(i.optName)(i.classifiedResult) + 1)
      s.updated(i.optName, trans)
    }

    table_filled.foreach{x => printRow(x._1)(x._2)}
  }

  def printVQRSimple = {
    val VblankRow = new scala.collection.immutable.HashMap[LLVMBerryLogics.VResult, Int]() +
    ((LLVMBerryLogics.VSuccess, 0)) +
    ((LLVMBerryLogics.VFail, 0)) +
    ((LLVMBerryLogics.VNotSupported, 0)) +
    ((LLVMBerryLogics.VAdmitted, 0)) +
    ((LLVMBerryLogics.VAssertionFail, 0)) +
    ((LLVMBerryLogics.VUnknown, 0))

    // val table: Map[LLVMBerryLogics.VResult, Int] =
    //   new scala.collection.immutable.HashMap[LLVMBerryLogics.VResult, Int]().
    //     withDefaultValue(0)
    val table = VblankRow
    val table_filled = VQR.foldLeft(table){(s, i) =>
      s.updated(i.classifiedResult, s(i.classifiedResult) + 1)
    }
    printRow("All Validation")(table_filled)
  }
}

MainScript.main(args)
