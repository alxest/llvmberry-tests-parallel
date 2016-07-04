import sys.process._
import java.util.concurrent.atomic._
import java.util.concurrent.atomic.AtomicLong
import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent._
import scala.util.Random
import java.io.File
import java.nio.file.{Paths, Files}
import java.nio.charset.StandardCharsets
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
    def getPercentData = {
      val sum = data.values.foldLeft(0: Long)((s, i) => s + i)
      data.map(x => (x._1, "%.1f".format(100 * x._2.toDouble / sum) + "%"))
    }
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









class LLVMBerryLogics(
  val simplberry_path: Option[String],
  val opt_path: String,
  val main_native_path: String,
  val opt_arg: String,
  val input_test_dir: String,
  val output_result_dir: String) {

  // it seems I cannot put some code here..
  // def this(x: Map[Symbol, String]) = {
  //   this("", "", "", "", "", "") 
  // }

  def compile = {
    val generator_compile = exec(s"cd ${simplberry_path}/ && make opt -j24")
    if(generator_compile._1 != 0) {
      println("Compile Failed!")
      println("stdout : " + generator_compile._2)
      println("stderr : " + generator_compile._3)
      assert(false)
    }
    val validator_compile = exec(s"cd ${simplberry_path}/ && make refact -j24")
    if(validator_compile._1 != 0) {
      println("Compile Failed!")
      println("stdout : " + validator_compile._2)
      println("stderr : " + validator_compile._3)
      assert(false)
    }
  }

  def generate(ll_base: String) = {
    val cmd = s"${opt_path} ${opt_arg} ${ll_base}.ll" +
    s"-o ${ll_base}.${LLVMBerryLogics.OUT_NAME}.ll -S"
    exec(cmd)
  }

  def validate(triple_base: String, debug: Boolean) = {
    val src = triple_base + ".src.bc"
    val tgt = triple_base + ".tgt.bc"
    val hint = triple_base + ".hint.json"
    val cmd = s"${main_native_path} ${if(debug) "-d" else ""} ${src} ${tgt} ${hint}"
    exec(cmd)
  }

  def cleanByProducts = {
    ??? //this is deprecated, but leave it for now
    exec(s"""cd ${input_test_dir} && find . -name "*.src.bc" -delete""")
    exec(s"""cd ${input_test_dir} && find . -name "*.tgt.bc" -delete""")
    exec(s"""cd ${input_test_dir} && find . -name "*.src.ll" -delete""")
    exec(s"""cd ${input_test_dir} && find . -name "*.tgt.ll" -delete""")
    exec(s"""cd ${input_test_dir} && find . -name "*.output.ll" -delete""")
    exec(s"""cd ${input_test_dir} && find . -name "*.result" -delete""")
  }
}

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

  val OUT_NAME = "output"
  val NOT_PLAINS: List[String] = s"src.ll tgt.ll $OUT_NAME.ll".split(" ").toList

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










class TestRunner(val ll_bases: List[String], val num_threads: Int) {
  object Mutex
  sealed abstract class Job
  case class GQJob(val ll_base: String) extends Job
  case class VQJob(val triple_base: String) extends Job
  case object Nothing extends Job
  case object Terminate extends Job

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


  val GQ = new ConcurrentLinkedQueue[String]
  val VQ = new ConcurrentLinkedQueue[String]

  var GQR = scala.collection.mutable.Queue[GQJobResult]()
  var VQR = scala.collection.mutable.Queue[VQJobResult]()

  var GQ_total = 0
  var VQ_current_total = 0
  def VQ_estimated_total: Double = {
    val num_generated = GQR.foldLeft(0)((s, i) => s + i.generated)
    num_generated * Math.pow((1.0 * GQ_total / GQR.size), 3.0)
  }
  def GQ_total_time_elapsed =
    GQR.foldLeft(0.0)((s, i) => s + i.time)
  def VQ_total_time_elapsed =
    VQR.foldLeft(0.0)((s, i) => s + i.time)
  def GQ_estimated_single_time = GQ_total_time_elapsed / GQR.size
  def VQ_estimated_single_time = VQ_total_time_elapsed / VQR.size
  def GQ_estimated_ETA = GQ_estimated_single_time * (GQ_total - GQR.size)
  def VQ_estimated_ETA = VQ_estimated_single_time * (VQ_estimated_total - VQR.size)

  // var count = 0

  def fetchNextJob: Job = {
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
      val res = TimeChecker.runWithClock("GQ#generate") {
        LLVMBerryLogics.generate(ll_base)
      }
      val tri_bases = LLVMBerryLogics.get_triple_bases(ll_base)
      tri_bases.foreach(VQ.offer(_))
      Mutex.synchronized { VQ_current_total += tri_bases.size }
      val t1 = System.currentTimeMillis
      val exitRes = LLVMBerryLogics.classifyGenerateResult(res)
      if(exitRes != LLVMBerryLogics.GSuccess) {
        import java.nio.file.{Paths, Files}
        import java.nio.charset.StandardCharsets
        val result = "########## STDOUT\n" + res._2 +
        "\n\n\n ########## STDERR\n" + res._3
        TimeChecker.runWithClock("GQ#write result") {
          Files.write(Paths.get(ll_base + ".result"),
            result.getBytes(StandardCharsets.UTF_8))
        }
      }
      new GQJobResult(fileSize, (t1 - t0)/1000.0, tri_bases.size, exitRes)
    }
  }

  def processVQ(triple_base: String): VQJobResult = {
    TimeChecker.runWithClock("VQ") {
      val t0 = System.currentTimeMillis
      val fileSize = (new File(triple_base + ".ll")).length
      var res = TimeChecker.runWithClock("VQ#validate") {
        LLVMBerryLogics.validate(triple_base, false)
      }
      val optName = LLVMBerryLogics.getOptName(triple_base)
      val t1 = System.currentTimeMillis
      val exitRes = LLVMBerryLogics.classifyValidateResult(res)
      if(exitRes != LLVMBerryLogics.VSuccess) {
        TimeChecker.runWithClock("VQ#validate with debug") {
          res = LLVMBerryLogics.validate(triple_base, true)
        }
        val result = "########## STDOUT\n" + res._2 +
        "\n\n\n ########## STDERR\n" + res._3
        TimeChecker.runWithClock("VQ#write result") {
          Files.write(Paths.get(triple_base + ".result"),
            result.getBytes(StandardCharsets.UTF_8))
        }
        TimeChecker.runWithClock("VQ#llvm-dis") {
          exec(s"llvm-dis ${triple_base}.src.bc")
          exec(s"llvm-dis ${triple_base}.tgt.bc")
        }
      }
      else {
        TimeChecker.runWithClock("VQ#remove triple") {
          Files.delete(Paths.get(triple_base + ".src.bc"))
          Files.delete(Paths.get(triple_base + ".tgt.bc"))
          Files.delete(Paths.get(triple_base + ".hint.json"))
        }
      }
      new VQJobResult(fileSize, (t1 - t0)/1000.0, optName, exitRes)
    }
  }

  class MyThread extends Thread {
    override def run {
      def runner(): Unit = {
        printBrief
        fetchNextJob match {
          case GQJob(ll_base) =>
            val res = processGQ(ll_base)
            Mutex.synchronized { GQR += res }
            runner
          case VQJob(triple_base) =>
            val res = processVQ(triple_base)
            Mutex.synchronized { VQR += res }
            runner
          case Nothing => Thread.sleep(2000) ; runner
          case Terminate => ()
        }
      }
      runner()
    }
  }

  var last_printed: Long = 0

  def printBrief = Mutex.synchronized {
    TimeChecker.runWithClock("printBrief") {
      val t0 = System.currentTimeMillis()
      if(t0 - last_printed > 250) {
        last_printed = t0
        (1 to 6) foreach { _ => goPreviousLine }
        println((GQR.size + "/" + GQ_total).padTo(30, ' ') +
          "%.1f".format(GQ_estimated_ETA))
        println((VQR.size + "/" + VQ_estimated_total).padTo(30, ' ') +
          "%.1f".format(VQ_estimated_ETA))
        println("####" + VQ_current_total + " " + VQ_estimated_total)
        printGQR
        printVQRSimple
        // println(TimeChecker.data)
        println(TimeChecker.getPercentData)
      }
    }
  }

  def printRow[A](row_name: String)(table: Map[A, Int]) = {
    print(row_name.padTo(20, ' ') + " ---->   ")
    table.foreach(y => print((if(y._2 != 0) y.toString else "").padTo(20, ' ') + " "))
    println
  }

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

  def run = {
    ll_bases.foreach(GQ.offer(_))
    GQ_total = GQ.size
    val threads: IndexedSeq[Thread] =
      (for (i <- 1 to num_threads) yield {
        val thread = new MyThread
        thread.start
        thread
      })
    threads.foreach(_.join)
    assert(GQ.size == 0 && GQR.size == GQ_total)
    assert(VQ.size == 0 && VQR.size == VQ_current_total)
  }
}

//http://stackoverflow.com/questions/2315912/scala-best-way-to-parse-command-line-parameters-cli
object MainScript {

  //http://stackoverflow.com/questions/9725675/is-there-a-standard-format-for-command-line-shell-help-text
  //http://docopt.org/
  val usage = """
What It Do:
    *TODO*

Usage:

-h, --help:
    Show help text. This.

-s, --simplberry-path <path>:
    Set simplberry directory path.
    Default is set to ../, assuming this script is inside simplberry-tests dir.
    *NOTE*
    This path is used to get --opt-path and --vali-path.
    You should not specify this with --opt-path or --vali-path at the same time.

--opt-path <path>:
    Set path of the "opt".
    *NOTE*
    By specifying this value, it will not get commit/patch info of lib/llvm.
    Which means the test result will not be self-contained.
    Re-constructing the same setting just from the test result will not be able.

--vali-path <path>:
    Set path of the validator, "main.native".
    *NOTE*
    By specifying this value, it will not get commit/patch info of lib/llvm.
    Which means the test result will not be self-contained.
    Re-constructing the same setting just from the test result will not be able.

-a, --opt-arg <string>:
    Set parameter used in opt.
    Default value is O2.

-i, --input-path <path>:
    Set input test directory.

-o, --output-path <path>:
    Set resulting output directory.
    In default, directory is automatically created inside current directory.

-p, --process-strategy <g|v|d>:
    Set job processing strategy.
    "g": try generate(opt) first.
    "v": try validate(main.native) first.
    "d": default, balancing according to estimated time left.
    Default value is "d".

-g, --generate-strategy:
    *TODO*
    If we are interested in specific pass, running entire O2 with our opt is
    not that meaningfull.
    As it is not stabilized yet, it may cause assertion failure.
    Also it takes far more time writing all the triples.
    Therefore, it would be good to run all passes that appear earlier than
    "the pass" with basic LLVM opt, and then run "the pass" with our opt.

-v, --validate-strategy <f|s|d>:
    Set validating strategy.
    "f": Fast. Always run without "-d" option.
    "s": Slow. Always run with "-d" option, and llvm-dis to src/tgt.
    "d": First try with "f" strategy, and then if validation has not succeeded,
         re-run with "s" strategy.
    Default value is "d"
  """
  //TODO llvm-dis path?

  type option_map = Map[Symbol, String]

  def parse_option(args: Array[String]): option_map = {
    def nextOption(map : option_map, list: List[String]) : option_map = {
      list match {
        case Nil => map
        case ("-h" | "--help") :: _ =>
          println(usage)
          System.exit(0)
          ???
        case ("-s" | "--simplberry-path") :: value :: tail =>
          nextOption(map ++ Map('s -> value), tail)
        case "--opt-path"  :: value :: tail =>
          nextOption(map ++ Map('optpath -> value), tail)
        case "--vali-path"  :: value :: tail =>
          nextOption(map ++ Map('valipath-> value), tail)
        case ("-a" | "--opt-arg") :: value :: tail =>
          nextOption(map ++ Map('a -> value), tail)
        case ("-i" | "--input-path") :: value :: tail =>
          nextOption(map ++ Map('i -> value), tail)
        case ("-o" | "--output-path") :: value :: tail =>
          nextOption(map ++ Map('o -> value), tail)
        case ("-p" | "--process-strategy") :: value :: tail =>
          nextOption(map ++ Map('p -> value), tail)
        case ("-g" | "--generate-strategy") :: value :: tail =>
          println("Not implemented yet : -g")
          System.exit(1)
          ???
        case ("-v" | "--validate-strategy") :: value :: tail =>
          nextOption(map ++ Map('v -> value), tail)
        case option :: _ =>
          println("Unknown option : " + option)
          System.exit(1)
          ???
      }
    }
    // val bb = ("-s" | "--simplberry-path") --> error
    // "|" seems correctly interpreted
    nextOption(Map(), args.toList)
  }

  def main(args: Array[String]) = {
    val option_map = parse_option(args)
    def mkLLVMBerryLogics = (new LLVMBerryLogics(_, _, _, _, _, _)).curried
    val llvmberry_logics = {
      val simplberry_path = option_map.get('s)
      val t0 = mkLLVMBerryLogics(simplberry_path)
      val t1 =
        simplberry_path match {
          case Some(spth) =>
            val opt_path = spth + "/.build/llvm-obj/bin/opt"
            val vali_path = spth + "/ocaml_refact/main.native"
            t0(opt_path)(vali_path)
          case None =>
            t0(option_map.get('optpath).get)(
              option_map.get('valipath).get)
        }
      val opt_arg = option_map.get('a).getOrElse("-O2")
      val t2 = t1(opt_arg)
      val input_test_dir = option_map.get('i).get
      val t3 = t2(input_test_dir)
      val output_result_dir = {
        val ls = exec("ls")._2.split('\n')
        val y = input_test_dir.split('/').last
        def go(i: Int): String = {
          val x = "test_result#" + y + "#" + i
          if(ls.contains(x))
            go(i+1)
          else x
        }
        go(0)
      }
      val t4 = t3(output_result_dir)
      t4
    }

    println(llvmberry_logics.output_result_dir)
    exec(s"cp -R ${llvmberry_logics.input_test_dir} ${llvmberry_logics.output_result_dir}")
    val runner = new TestRunner(LLVMBerryLogics.get_ll_bases(llvmberry_logics.output_result_dir), 24)
    println ; println ; printBar()
    println("Start Script")
    llvmberry_logics.compile
    println("Compile Done")
    llvmberry_logics.cleanByProducts
    println("cleanByProducts Done")
    for(i <- 1 to 12) println
    runner.run
    for(i <- 1 to 8) println
    println("Test Done")
    println(runner.GQ_total + " " + runner.VQ_current_total)
    println ; println ; printBar()
    runner.printGQR
    println ; println ; printBar()
    runner.printVQR
    println ; println ; printBar()
    runner.printVQRSimple
    println ; println ; printBar()
  }
}

MainScript.main(args)
