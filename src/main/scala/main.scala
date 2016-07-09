import sys.process._
import java.io.File
import scala.annotation.tailrec
// import org.scalatest._

object CommonLogics {

  def format_double(x: Double): String =
    "%.1f".format(x)

  def printToFile(f: java.io.File)(op: java.io.PrintWriter => Unit) {
    val p = new java.io.PrintWriter(f)
    try { op(p) } finally { p.close() }
  }

  //It does not have to be file, rather it can be String
  //but differentiating type can far prevent error... really.
  //also we can always wrap to path object if it is really a path.
  //originally used nio, but changed to this
  def write_to_file(contents: String, file: File): Unit = {
    TimeChecker.runWithClock("write_to_file") {
      import java.io.PrintWriter
      val p = new PrintWriter(file)
      try {
        p.write(contents)
      }
      catch {
        case e: Throwable =>
          println(e)
          println("File Write Failed!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
          for(_ <- 1 to 20) println("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
          //just terminating here will only terminate one thread.
          //TODO create error logger?
      }
      finally {
        p.close
      }
    }
  }

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

  def string_with_bar(x: String = ""): String = {
    val width = 190
    val pad = width - x.length
      // exec("tput cols")._2.trim.toInt - x.size
    val half_pad = pad/2
    "-" * half_pad + x + "-" * (pad - half_pad)
  }

  object TimeChecker {
      // new scala.collection.immutable.HashMap[String, Map[String, Int]]().
    private var data: scala.collection.mutable.Map[String, Long] =
      new scala.collection.mutable.HashMap[String, Long]().withDefaultValue(0)
    val checkTime = true
    //Percent data is actually not that meaningful,
    //as it is not called mutually exclusive
    //Just for convenience
    def getPercentData = {
      val sum = data.values.foldLeft(0: Long)((s, i) => s + i)
      data.map(x => (x._1, format_double(100 * x._2.toDouble / sum) + "%"))
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









//get parameter and move to static object?
class LLVMBerryLogics(option_map: Map[Symbol, String]) {
  import LLVMBerryLogics._

  val initializer = {
    val simplberry_path = option_map.get('s)
    val opt_path = option_map.get('optpath)
    val vali_path = option_map.get('valipath)
    (simplberry_path, opt_path, vali_path) match {
      case (Some(spth), None, None) =>
        val opt_path = spth + "/.build/llvm-obj/bin/opt"
        val vali_path = spth + "/ocaml_refact/main.native"
        (Some(spth), opt_path, vali_path)
      case (None, Some(opth), Some(vpth)) =>
        (None, opth, vpth)
      case (None, None, None) =>
        val spth: String = exec("cd .. && pwd")._2.stripLineEnd
        val opt_path = spth + "/.build/llvm-obj/bin/opt"
        val vali_path = spth + "/ocaml_refact/main.native"
        (Some(spth), opt_path, vali_path)
      case _ =>
        Console.err.println("""Only three cases are possible.
1. Only specify simplberry path
2. Only specify opt_path and vali_path
3. Do not speecify at all.
""")
        ???
    }
  }

  val simplberry_path = initializer._1
  val opt_path = initializer._2
  val main_native_path = initializer._3
  val opt_arg = option_map.get('a).getOrElse("-O2")
  val input_test_dir = option_map.get('i).get
  val generate_strategy = option_map.get('g).getOrElse("d")
  val validate_strategy = option_map.get('v).getOrElse("d")

  val output_result_dir = {
    val ls = exec("ls")._2.split('\n')
    val y = input_test_dir.split('/').last
    def go(i: Int): String = {
      val x = "test_result." + y + "." + i
      if(ls.contains(x))
        go(i+1)
      else x
    }
    go(0)
  }

  def compile = {
    simplberry_path match {
      case None => println("""Should not occur. Trying to compile
opt and main.native while --simplberry-path is not specified.""")
      case Some(spth) =>
        val generator_compile = exec(s"cd ${spth}/ && make opt -j24")
        if(generator_compile._1 != 0) {
          println("Compile Failed!")
          println("stdout : " + generator_compile._2)
          println("stderr : " + generator_compile._3)
          assert(false)
        }
        val validator_compile = exec(s"cd ${spth}/ && make refact -j24")
        if(validator_compile._1 != 0) {
          println("Compile Failed!")
          println("stdout : " + validator_compile._2)
          println("stderr : " + validator_compile._3)
          assert(false)
        }

        def write_status(git_path: String, write_path: String) = {
          val commit = exec(s"cd ${git_path} && git show HEAD")._2
          val diff = exec(s"cd ${git_path} && git diff HEAD")._2
          val txt =
            string_with_bar("COMMIT") + "\n" + commit + "\n\n" +
          string_with_bar("DIFF") + "\n" + diff + "\n\n"
          write_to_file(txt, new File(write_path))
        }

        write_status(s"${spth}/lib/llvm",
          output_result_dir + "/llvm.status")
        write_status(s"${spth}/",
          output_result_dir + "/simplberry.status")
        write_status(s"${spth}/lib/vellvm",
          output_result_dir + "/vellvm.status")
    }
  }

  def generate(ll_base: String): GResult = {
    TimeChecker.runWithClock("G") {
      val cmd = s"${opt_path} ${opt_arg} ${ll_base}.ll" +
      s" -o ${ll_base}.${LLVMBerryLogics.OUT_NAME}.ll -S"
      val res = exec(cmd)
      val gres = LLVMBerryLogics.classifyGenerateResult(res)
      // if(gres != LLVMBerryLogics.GSuccess) {
        val txt =
          string_with_bar("CMD") + "\n" + cmd + "\n\n" +
        string_with_bar("STDOUT") + "\n" + res._2 + "\n\n" +
        string_with_bar("STDERR") + "\n" + res._3 + "\n\n"
        write_to_file(txt, new File(ll_base + ".result"))
      // }
      gres
    }
  }

  def validate(triple_base: String): VResult = {
    TimeChecker.runWithClock("V") {
      val src = triple_base + ".src.bc"
      val tgt = triple_base + ".tgt.bc"
      val hint = triple_base + ".hint.json"

      TimeChecker.runWithClock("V#l-swtch") {
        exec(s"opt -lowerswitch ${src} -o ${src}")
        exec(s"opt -lowerswitch ${tgt} -o ${tgt}")
      }

      def get_cmd(dbg: Boolean): String =
        s"${main_native_path} ${if(dbg) "-d" else ""} ${src} ${tgt} ${hint}"

      lazy val cmd_no_dbg = get_cmd(false)
      lazy val cmd_dbg = get_cmd(true)

      def llvm_dis =
        TimeChecker.runWithClock("V#llvm_dis") {
          exec(s"llvm-dis ${src}")
          exec(s"llvm-dis ${tgt}")
        }

      def rm_triple =
        TimeChecker.runWithClock("V#rm_triple") {
          (new File(src)).delete
          (new File(tgt)).delete
          (new File(hint)).delete
        }

      val vres = validate_strategy match {
        case "f" =>
          val vres = classifyValidateResult(exec(cmd_no_dbg))
          if(vres == LLVMBerryLogics.VSuccess)
            rm_triple
          vres
        // case "s" =>
        //   val res = exec(cmd_dbg)
        //   val vres = classifyValidateResult(res)
        //   if(vres == LLVMBerryLogics.VSuccess)
        //     rm_triple
        //   else {
        //     llvm_dis
        //     val txt =
        //       string_with_bar("CMD") + "\n" + cmd_dbg + "\n\n" +
        //     string_with_bar("STDOUT") + "\n" + res._2 + "\n\n" +
        //     string_with_bar("STDERR") + "\n" + res._3 + "\n\n"
        //     write_to_file(txt, new File(triple_base + ".result"))
        //   }
        //   vres
        case "d" =>
          val vres =
            TimeChecker.runWithClock("V#no_dbg") {
              classifyValidateResult(exec(cmd_no_dbg))
            }

          if(vres == LLVMBerryLogics.VSuccess)
            rm_triple
          else {
            //source , sink
            // val proc: ProcessBuilder = stringToProcess(cmd_dbg)
            TimeChecker.runWithClock("V#dbg") {
              stringSeqToProcess(
                Seq("/bin/sh",
                  "-c",
                  "${cmd_dbg} 2> ${triple_base}.dbg_result")).!
              // val boo = s"/bin/sh -c ${cmd_dbg} 2> ${triple_base}.dbg_result"
              // println(boo)
              // stringToProcess(boo).!
              // val proc: ProcessBuilder = stringToProcess(cmd_dbg)
              // (stringToProcess("ls") #> new File("tmp")).!
            }

            // val res =
            //   TimeChecker.runWithClock("V#dbg") {
            //     exec(cmd_dbg)
            //   }
            // val txt =
            //   string_with_bar("CMD") + "\n" + cmd_dbg + "\n\n" +
            // string_with_bar("STDOUT") + "\n" + res._2 + "\n\n" +
            // string_with_bar("STDERR") + "\n" + res._3 + "\n\n"
            // write_to_file(txt, new File(triple_base + ".result"))
          }
          vres
      }
      vres
    }
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

  def get_ll_bases(dir_name: String): List[String] = {
    val cmd = s"find ${dir_name} -name \\*.ll"
    val ret = exec(cmd)._2.split("\n").map(remove_extensions(1))
    ret.toList
    // scala.util.Random.shuffle(ret.toList)
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

  def get_opt_name(triple_base: String): String = {
    TimeChecker.runWithClock("get_opt_name") {
      import scala.util.parsing.json._
      try {
        val hint = scala.io.Source.fromFile(triple_base + ".hint.json").mkString
        val json = JSON.parseRaw(hint).get.asInstanceOf[JSONObject].obj
        json.get("opt_name").get.asInstanceOf[String]
      }
      catch {
        case e:Throwable =>
          println(e)
          println("json parsing error!!!!!!!!!!!!!!!!!!!" + triple_base)
          for(_ <- 1 to 20) println("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
          //just terminating here will only terminate one thread.
          //TODO create error logger?
          "no opt name"
      }
    }
  }
}










class TestRunner(
  val llvmberry_logics: LLVMBerryLogics,
  val option_map: Map[Symbol, String]) {

  import java.util.concurrent.atomic._
  import java.util.concurrent.atomic.AtomicLong
  import java.util.concurrent.atomic.AtomicReference
  import java.util.concurrent._

  val process_strategy = option_map.get('p).getOrElse("d")
  val num_threads = option_map.get('j).getOrElse("24").toInt

  object Mutex
  sealed abstract class Job
  case class GQJob(val ll_base: String) extends Job
  case class VQJob(val triple_base: String) extends Job
  case object NoJob extends Job
  case object Terminate extends Job

  sealed abstract class JobResult {
    val base_name: String
    val fileSize: Long
    val time: Double
    // val classifiedResult: String

    // f"size: ${fileSize}%20s" +
    //padTo(' ', 20) PASSES type checking!!!!!
    def p(x: String): String = x.padTo(25, ' ')
    override def toString =
      s"${base_name}".padTo(115, ' ') +
    p(s"   size: ${fileSize}") +
    p(s"   time: ${format_double(time)}")
    //sometimes base_name is too long and padding cannot care it.
    //there should be spaces between them
  } //without val, it is private

  class GQJobResult(
    val base_name: String,
    val fileSize: Long,
    val time: Double,
    val generated: Int,
    val classifiedResult: LLVMBerryLogics.GResult
  ) extends JobResult {
    override def toString: String = {
      super.toString +
      p(s"generated: ${generated}")
    }
  }

  class VQJobResult(
    val base_name: String,
    val fileSize: Long,
    val time: Double,
    val optName: String,
    val classifiedResult: LLVMBerryLogics.VResult
  ) extends JobResult {
    override def toString: String = {
      super.toString +
      p(s"optName: ${optName}")
    }
  }
  val GQ = new ConcurrentLinkedQueue[String]
  val VQ = new ConcurrentLinkedQueue[String]

  var GQR = scala.collection.mutable.Queue[GQJobResult]()
  var VQR = scala.collection.mutable.Queue[VQJobResult]()

  var GQ_total = 0 //TODO change it to val
  var VQ_current_total = 0
  def VQ_estimated_total: Double = {
    val num_generated = GQR.foldLeft(0)((s, i) => s + i.generated)
    num_generated * Math.pow((1.0 * GQ_total / GQR.size), 1.3)
  }
  def GQ_total_time_elapsed =
    GQR.foldLeft(0.0)((s, i) => s + i.time)
  def VQ_total_time_elapsed =
    VQR.foldLeft(0.0)((s, i) => s + i.time)
  def GQ_estimated_single_time = GQ_total_time_elapsed / GQR.size
  def VQ_estimated_single_time = VQ_total_time_elapsed / VQR.size
  def GQ_estimated_ETA = GQ_estimated_single_time * (GQ_total - GQR.size)
  def VQ_estimated_ETA = VQ_estimated_single_time * (VQ_estimated_total - VQR.size)

  def fetch_next_job: Job = {
    TimeChecker.runWithClock("fetch_next_job") {
      if(GQR.size == GQ_total && VQR.size == VQ_current_total) Terminate
      else {
        def tryGQ: Job = {
          val bb = Option(GQ.poll)
          if(bb.isDefined) GQJob(bb.get)
          else NoJob
        }

        def tryVQ: Job = {
          val bb = Option(VQ.poll)
          if(bb.isDefined) VQJob(bb.get)
          else NoJob
        }

        var ret: Job = NoJob
        process_strategy match {
          case "g" =>
            if(ret == NoJob) ret = tryGQ
            if(ret == NoJob) ret = tryVQ
          case "v" =>
            if(ret == NoJob) ret = tryVQ
            if(ret == NoJob) ret = tryGQ
          case "d" =>
            ret = if(GQ_estimated_ETA > VQ_estimated_ETA)
              tryGQ
            else
              tryVQ
            if(ret == NoJob) ret = tryGQ
            if(ret == NoJob) ret = tryVQ
          case _ =>
            assert(false)
            ???
        }
        ret
      }
    }
  }

  def processGQ(ll_base: String): GQJobResult = {
    TimeChecker.runWithClock("processGQ") {
      val t0 = System.currentTimeMillis
      val fileSize = (new File(ll_base + ".ll")).length
      val gres = llvmberry_logics.generate(ll_base)
      val tri_bases = LLVMBerryLogics.get_triple_bases(ll_base)
      tri_bases.foreach(VQ.offer(_))
      Mutex.synchronized { VQ_current_total += tri_bases.size }
      val t1 = System.currentTimeMillis
      new GQJobResult(ll_base, fileSize, (t1 - t0)/1000.0, tri_bases.size, gres)
    }
  }

  def processVQ(triple_base: String): VQJobResult = {
    TimeChecker.runWithClock("processVQ") {
      val t0 = System.currentTimeMillis
      val fileSize = (new File(triple_base + ".ll")).length
      val optName = LLVMBerryLogics.get_opt_name(triple_base)
      val vres = llvmberry_logics.validate(triple_base)
      val t1 = System.currentTimeMillis
      new VQJobResult(triple_base, fileSize, (t1 - t0)/1000.0, optName, vres)
    }
  }

  class MyThread extends Thread {
    override def run {
      @tailrec
      def runner(): Unit = {
        print_progress
        fetch_next_job match {
          case GQJob(ll_base) =>
            val res = processGQ(ll_base)
            Mutex.synchronized { GQR += res }
            runner
          case VQJob(triple_base) =>
            val res = processVQ(triple_base)
            Mutex.synchronized { VQR += res }
            runner
          case NoJob => Thread.sleep(1000) ; runner
            //NoJob Logger?
          case Terminate => ()
        }
      }
      runner()
    }
  }

  def run = {
    val ll_bases =
      LLVMBerryLogics.get_ll_bases(llvmberry_logics.output_result_dir)
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

  //TODO separate object
  var last_printed: Long = 0

  def print_progress = Mutex.synchronized {
    TimeChecker.runWithClock("print_progress") {
      val t0 = System.currentTimeMillis()
      if(t0 - last_printed > 250) {
        last_printed = t0
        (1 to 7) foreach { _ => goPreviousLine }
        println((GQR.size + "/" + GQ_total).padTo(30, ' ') +
          format_double(GQ_estimated_ETA))
        println((VQR.size + "/" + format_double(VQ_estimated_total)).padTo(30, ' ') +
          format_double(VQ_estimated_ETA))
        println("####" + VQ_current_total + " " + format_double(VQ_estimated_total))
        println(GQR_to_row)
        println(VQR_to_row)
        // println(TimeChecker.data)
        val dat = TimeChecker.getPercentData
        val dat_ = dat.splitAt(dat.size/2)
        println(dat_._1 + "\n" + dat_._2)
      }
    }
  }

  def row_to_string[A](row_name: String)(table: Map[A, Int]): String = {
    val sum = table.values.sum
    val t0 = row_name.padTo(20, ' ') + s"(${sum})".padTo(10, ' ') +
    " ---->   "
    val t1 = table.foldRight(t0)((i, s) =>
      s + (if(i._2 != 0) i.toString else "").padTo(20, ' ') + " ")
    t1
  }

  def GQR_to_row: String = {
    val table: Map[LLVMBerryLogics.GResult, Int] =
      new scala.collection.immutable.HashMap[LLVMBerryLogics.GResult, Int]().
        withDefaultValue(0)
    val table_filled = GQR.foldLeft(table){(s, i) =>
      s.updated(i.classifiedResult, s(i.classifiedResult) + 1)
    }
    row_to_string("All Generation")(table_filled)
  }

  def VQR_to_matrix = {
    val VblankRow = new scala.collection.immutable.HashMap[LLVMBerryLogics.VResult, Int]() +
    ((LLVMBerryLogics.VSuccess, 0)) +
    ((LLVMBerryLogics.VFail, 0)) +
    ((LLVMBerryLogics.VNotSupported, 0)) +
    ((LLVMBerryLogics.VAdmitted, 0)) +
    ((LLVMBerryLogics.VAssertionFail, 0)) +
    ((LLVMBerryLogics.VUnknown, 0))

    val table: Map[String, Map[LLVMBerryLogics.VResult, Int]] =
      new scala.collection.immutable.HashMap[String, Map[LLVMBerryLogics.VResult, Int]]().
        withDefaultValue(VblankRow)


    val table_filled = VQR.foldLeft(table){(s, i) =>
      val trans = s(i.optName).updated(
        i.classifiedResult,
        s(i.optName)(i.classifiedResult) + 1)
      s.updated(i.optName, trans)
    }

    table_filled.toList.sortBy(_._1).reverse.
      foldRight("")((x, s) => s + row_to_string(x._1)(x._2) + "\n")
  }

  def VQR_to_row: String = {
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
    row_to_string("All Validation")(table_filled)
  }


  //TODO get absolute path
  //TODO check time consumption
  //TODO check tolerance on big size
  //TODO print once more with optName as first index?
  def GQR_to_list: String =
    GQR.groupBy(_.classifiedResult).
      foldRight("")((i, s) =>
        s + string_with_bar(i._1.toString) + "\n\n" +
          i._2.foldLeft("")((s, i) => (s + i + "\n")) + "\n\n"
      )
  // GQR.sortBy(_.classifiedResult.toString).toList.
  // foldRight("")((i, s) => s + i.toString + "\n")
  //without toList, it causes stack overflow
  //same foldRight name but implementation changes

  def VQR_to_list: String = {
    def idx1(x: VQJobResult) = x.classifiedResult
    def idx2(x: VQJobResult) = x.optName
    VQR.groupBy(idx1).
      foldRight("")((i, s) =>
        s + string_with_bar(i._1.toString) + "\n\n" +
          i._2.sortBy(idx2).foldLeft("")((s, i) => (s + i + "\n")) + "\n\n"
      )
  }
      // (x => (x._1, x._2.groupBy(_.classifiedResult))).
      // foldRight("")((i, s) => s + i.toString + "\n")
  // VQR.groupBy(_.optName).map(x => (x._1, x._2.groupBy(_.classifiedResult))).
  //   foldRight("")((i, s) => s + i.toString + "\n")
  // VQR.sortBy(x => (x.optName, x.classifiedResult.toString)).toList.
  // foldRight("")((i, s) => s + i.toString + "\n")
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
    By specifying this value, the script will not get commit/patch info of lib/llvm.
    Which means the test result dir will not be self-contained.
    Re-constructing the same setting just from the test result dir will not be able.

--vali-path <path>:
    Set path of the validator, "main.native".
    *NOTE*
    By specifying this value, the script will not get commit/patch info of lib/llvm.
    Which means the test result dir will not be self-contained.
    Re-constructing the same setting just from the test result dir will not be able.

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
    "d": Try with without "-d" option, and then if validation has not succeeded,
         re-run with "-d" option. It does not run llvm-dis.
    Default value is "d".

-j, --jobs <int>:
    Set number of threads.
    Default value is 24.
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
        case ("-j" | "--jobs") :: value :: tail =>
          nextOption(map ++ Map('j -> value), tail)
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

  def main(args: Array[String]): Unit = {
    val option_map = parse_option(args)
    val llvmberry_logics = new LLVMBerryLogics(option_map)

    exec(s"cp -R ${llvmberry_logics.input_test_dir} ${llvmberry_logics.output_result_dir}")
    val runner = new TestRunner(llvmberry_logics, option_map)


    println(llvmberry_logics.simplberry_path)
    println(llvmberry_logics.output_result_dir)
    println ; println ; println(string_with_bar())
    println("Start Script")
    if(llvmberry_logics.simplberry_path.isDefined) {
      llvmberry_logics.compile
      println("Compile Done")
    }
    for(i <- 1 to 12) println
    runner.run
    for(i <- 1 to 8) println
    println("Test Done")
    println(runner.GQ_total + " " + runner.VQ_current_total)

    //TODO separate report writing
    //it is more fault tolerant
    //also we may want to see report during main script is going
    val summary_txt =
      runner.GQR_to_row + "\n\n" + string_with_bar() + "\n" +
    runner.VQR_to_row + "\n\n" + string_with_bar() + "\n" +
    runner.VQR_to_matrix
    val detail_txt =
      string_with_bar() + "\n" + string_with_bar("Generate Result") + "\n" + string_with_bar() + "\n\n" +
    runner.GQR_to_list + "\n\n" +
    string_with_bar() + "\n" + string_with_bar("Validate Result") + "\n" + string_with_bar() + "\n\n" +
    runner.VQR_to_list + "\n\n"

    write_to_file(summary_txt, new File(llvmberry_logics.output_result_dir + "/report.summary"))
    write_to_file(detail_txt, new File(llvmberry_logics.output_result_dir + "/report.detail"))
    println("End Script")
  }
}









// val mb = 1024*1024
// val runtime = Runtime.getRuntime
// println("** Used Memory:  " + (runtime.totalMemory - runtime.freeMemory) / mb)
// println("** Free Memory:  " + runtime.freeMemory / mb)
// println("** Total Memory: " + runtime.totalMemory / mb)
// println("** Max Memory:   " + runtime.maxMemory / mb)
// for(_ <- 1 to 10) println

// MainScript.main(args)

