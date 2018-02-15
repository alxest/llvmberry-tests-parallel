import sys.process._
import java.io.File
import scala.annotation.tailrec
// import org.scalatest._

object CommonLogics {
  val DELIMITER = "\t"

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

  val COMMIT_HEADER = string_with_bar("COMMIT") + "\n"
  val DIFF_HEADER = string_with_bar("DIFF") + "\n"
  val CMD_HEADER = string_with_bar("CMD") + "\n"
  val STDOUT_HEADER = string_with_bar("STDOUT") + "\n"
  val STDERR_HEADER = string_with_bar("STDERR") + "\n"

  object TimeChecker {
      // new scala.collection.immutable.HashMap[String, Map[String, Int]]().
    private var data: scala.collection.mutable.Map[String, Long] =
      new scala.collection.mutable.HashMap[String, Long]().withDefaultValue(0)
    val checkTime = true
    //Percent data is actually not that meaningful,
    //as it is not called mutually exclusive
    //Just for convenience
    def getData = data
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
    def runWithClockSingle[A](block: => A): (A, Double) = {
      val t0 = System.currentTimeMillis()
      val result = block
      val t1 = System.currentTimeMillis()
      (result, (t1-t0).toDouble/1000)
    }
  }
}

import CommonLogics._









//get parameter and move to static object?
class CrellvmLogics(option_map: Map[Symbol, String]) {
  import CrellvmLogics._

  val initializer = {
    val simplberry_path = option_map.get('s)
    val opt_path = option_map.get('optpath)
    val vali_path = option_map.get('valipath)
    (simplberry_path, opt_path, vali_path) match {
      case (Some(spth), None, None) =>
        val opt_path = spth + "/.build/llvm-obj/bin/opt"
        val vali_path = spth + "/ocaml/main.native"
        (Some(spth), opt_path, vali_path)
      case (None, Some(opth), Some(vpth)) =>
        (None, opth, vpth)
      case (None, None, None) =>
        val spth: String = exec("cd .. && pwd")._2.stripLineEnd
        val opt_path = spth + "/.build/llvm-obj/bin/opt"
        val vali_path = spth + "/ocaml/main.native"
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
  val noresult: Boolean = option_map.contains('noresult)
  val report_nosuccess: Boolean = option_map.contains('report_nosuccess)

  val output_result_dir: String = {
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

  def copy_input_dir = {
    exec(s"cp -R ${input_test_dir} ${output_result_dir}")
  }

  def copy_executable = {
    exec(s"cp ${opt_path} ${output_result_dir}")
    exec(s"cp ${main_native_path} ${output_result_dir}")
  }

  def compile(spth: String) = {
    val generator_compile = exec(s"cd ${spth}/ && make opt -j24")
    if(generator_compile._1 != 0) {
      println("Compile Failed!")
      println("stdout : " + generator_compile._2)
      println("stderr : " + generator_compile._3)
      assert(false)
    }
    val validator_compile = exec(s"cd ${spth}/ && make exec-rsync -j24")
    if(validator_compile._1 != 0) {
      println("Compile Failed!")
      println("stdout : " + validator_compile._2)
      println("stderr : " + validator_compile._3)
      assert(false)
    }

    def write_status(git_path: String, write_path: String) = {
      val commit = exec(s"cd ${git_path} && git show HEAD")._2
      val diff = exec(s"cd ${git_path} && git diff HEAD")._2
      val txtbuilder = new StringBuilder(COMMIT_HEADER.length() +
                commit.length() + 2 + DIFF_HEADER.length() +
                diff.length() + 3)
      val txt = txtbuilder.append(COMMIT_HEADER).append(commit).
                append("\n\n").append(DIFF_HEADER).append(diff).
                append("\n\n").toString()
      write_to_file(txt, new File(write_path))
    }

    write_status(s"${spth}/lib/llvm",
      output_result_dir  + "/llvm.status")
    write_status(s"${spth}/",
      output_result_dir + "/simplberry.status")
    write_status(s"${spth}/lib/vellvm",
      output_result_dir + "/vellvm.status")
  }

  def generate(ll_base: String): (GResult, (Double, Double, Double, Double, Double, Double, Double, Double)) = {
    TimeChecker.runWithClock("G") {
      val cmd = s"${opt_path} -time-passes ${opt_arg} ${ll_base}.ll" +
      s" -o ${ll_base}.${CrellvmLogics.OUT_NAME}.ll -S"
      val res = exec(cmd)
      val gres = CrellvmLogics.classifyGenerateResult(res)
      // if(gres != CrellvmLogics.GSuccess) {
        val txtbuilder = new StringBuilder(CMD_HEADER.length() +
                  cmd.length() + 2 + STDOUT_HEADER.length() + res._2.length() +
                  2 + STDERR_HEADER.length() + res._3.length() + 3)
        val txt = txtbuilder.append(CMD_HEADER).append(cmd).
                  append("\n\n").append(STDOUT_HEADER).append(res._2).
                  append("\n\n").append(STDERR_HEADER).append(res._3).
                  append("\n\n").toString()
        if(!noresult)
          write_to_file(txt, new File(ll_base + ".result"))
      // }
      if(gres == GSuccess)
        (gres, parseTimeOutput(res._3))
      else
        (gres, (-2, -2, -2, -2, -2, -2, -2, -2))
    }
  }

  def validate(triple_base: String): (VResult, List[(Double, Double)]) = {
    TimeChecker.runWithClock("V") {
      val src = triple_base + ".src.bc"
      val tgt = triple_base + ".tgt.bc"
      val hint = triple_base + ".hint.json"

      TimeChecker.runWithClock("V#l-swtch") {
        exec(s"${opt_path} ${src} -o ${src}")
        exec(s"${opt_path} ${tgt} -o ${tgt}")
      }

      def get_cmd(dbg: Boolean): String =
        s"${main_native_path} -t ${if(dbg) "-d" else ""} ${src} ${tgt} ${hint}"

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

      //TODO only redirecting stderr. Should update main.native
      def run_dbg =
        TimeChecker.runWithClock("V#dbg") {
          stringSeqToProcess(
            Seq("/bin/sh",
              "-c",
              s"${cmd_dbg} > /dev/null 2> ${triple_base}.dbg_result")).!
        }
      //   TimeChecker.runWithClock("V#dbg") {
      //     val res =
      //       exec(cmd_dbg)
      //     val txt =
      //       string_with_bar("CMD") + "\n" + cmd_dbg + "\n\n" +
      //     string_with_bar("STDOUT") + "\n" + res._2 + "\n\n" +
      //     string_with_bar("STDERR") + "\n" + res._3 + "\n\n"
      //     write_to_file(txt, new File(triple_base + ".result"))
      //   }

      val res = exec(cmd_no_dbg)
      val vres = classifyValidateResult(res)
      val txtbuilder = new StringBuilder(CMD_HEADER.length() +
                cmd_no_dbg.length() + 2 + STDOUT_HEADER.length() +
                res._2.length() + 2 + STDERR_HEADER.length() +
                res._3.length() + 3)
      val txt = txtbuilder.append(CMD_HEADER).append(cmd_no_dbg).
                append("\n\n").append(STDOUT_HEADER).append(res._2).
                append("\n\n").append(STDERR_HEADER).append(res._3).
                append("\n\n").toString()
      if(!noresult)
        write_to_file(txt, new File(triple_base + ".result"))
      validate_strategy match {
        case "f" =>
          if(vres == CrellvmLogics.VSuccess)
            rm_triple
        case "d" =>
          if(vres == CrellvmLogics.VFail
            || vres == CrellvmLogics.VAssertionFail
            || vres == CrellvmLogics.VAdmitted)
            run_dbg
          if(vres == CrellvmLogics.VSuccess)
            rm_triple
        case "s" =>
          run_dbg
      }

      def parseTimeOutput(rawData: String) = {
        val x = res._3.split("\n")
          .filter(_.substring(0, 12) == "MEASURE_TIME")
          .map(_.substring(12).trim.split("\\s+"))
        assert(x.size == 5 ||
          {println("Error!!!!!!!!!!!!!!!!!\n" + rawData + "\n" + vres + "\n\n\n\n") ; false})
        for(i <- (0 until x.size)) {
          assert(x(i).size == 3 ||
            {println("Error!!!!!!!!!!!!!!!!!\n" + x(i).mkString("\t") + "\n\n\n\n") ; false})
        }
        x.map(i => (i(0).toDouble, i(1).toDouble)).toList
        // val table: Map[String, (Double, Double)] =
        //   new scala.collection.immutable.HashMap[String, (Double, Double)]()
        // val table_filled = x.foldLeft(table){(s, i) =>
        //   s.updated(i(2), (i(0).toDouble, i(1).toDouble))
        // }
        // println(x.map(_.mkString(" ")).mkString("\n") + "\n\n\n\n")
      }
      if(vres == VSuccess || vres == VFail)
        (vres, parseTimeOutput(res._3))
      else
        (vres, List.fill(5)((-1, -1)))
    }
  }
}

object CrellvmLogics {
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
    val retlist = ret.toList
    val retlist_withfilesz = retlist.map (x => ((new File(x + ".ll")).length, x))
    val retlist_sorted_withfsz = retlist_withfilesz.sortWith(_._1 > _._1)
    val retlist_sorted = retlist_sorted_withfsz.map(x => x._2)
    retlist_sorted
    // scala.util.Random.shuffle(ret.toList)
  }

  def remove_extensions(n: Int)(x: String): String =
    x.split('.').dropRight(n).mkString(".")

  def get_triple_bases(ll_base: String): List[String] = {
    val t = exec(s"ls ${ll_base}.*.*.src.bc")
    if(t._1 == 0) t._2.split("\n").map(remove_extensions(2)).toList
    else List()
  }

  def parseTimeOutput(rawData: String): (Double, Double, Double, Double, Double, Double, Double, Double) = {
    val content = {
      val tmp = rawData.split("\n")
      val startIdx = tmp.toList.indexWhere(_.contains("... Pass execution timing report ..."))
      tmp.drop(startIdx-1)
    }
    //slice: [)
    var InstCombineWallTime = 0.0
    var GVNWallTime = 0.0
    var SROAWallTime = 0.0
    var LICMWallTime = 0.0
    assert(content(content.length - 10).split("\\s+").last == "Total"
      || { println("#########################################\n" + rawData + "\n\n\n\n\n\n") ; false })
    val upperMostRowParsed = content(5).split(" ").map(_.filterNot(_ == '-')).filterNot(_ == "")
    

    assert((upperMostRowParsed.dropRight(2).last == "Wall" &&
      upperMostRowParsed.dropRight(1).last == "Time" &&
      upperMostRowParsed.last == "Name") ||
      { println("\n!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n" + rawData + "\n\n\n\n\n\n\n\n") ; false })
    val content2 = content.slice(6, content.length - 10).map{x =>
      // val y = x.split("\\s+")
      val y = x.split("[)]").map(_.trim)
      val time = y(y.size-2).split(" ").head.toDouble
      if(y.last == "Global Value Numbering") GVNWallTime += time
      else if(y.last == "Combine redundant instructions") InstCombineWallTime += time
      else if(y.last == "SROA") SROAWallTime += time
      else if(y.last == "Loop Invariant Code Motion") LICMWallTime += time 
      // y.mkString("--------")
      ()
    }
    if(upperMostRowParsed.contains("User+System")) {
      var InstCombineTime = 0.0
      var GVNTime = 0.0
      var SROATime = 0.0
      var LICMTime = 0.0
      assert(upperMostRowParsed.takeRight(4).toList == List("User+System", "Wall", "Time", "Name") ||
        { println("\n!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n" + rawData + "\n\n\n\n\n\n\n\n") ; false })
      val content2 = content.slice(6, content.length - 10).map{x =>
        // val y = x.split("\\s+")
        val y = x.split("[)]").map(_.trim)
        val time = y(y.size-3).split(" ").head.toDouble
        if(y.last == "Global Value Numbering") GVNTime += time
        else if(y.last == "Combine redundant instructions") InstCombineTime += time
        else if(y.last == "SROA") SROATime += time
        else if(y.last == "Loop Invariant Code Motion") LICMTime += time
        // y.mkString("--------")
        ()
      }
      (InstCombineWallTime, GVNWallTime, SROAWallTime, LICMWallTime, InstCombineTime, GVNTime, SROATime, LICMTime)
    }
    else (InstCombineWallTime, GVNWallTime, SROAWallTime, LICMWallTime, -1, -1, -1, -1)
  }


  def classifyGenerateResult(x: (Int, String, String)): GResult = {
    if(x._1 == 0) GSuccess
    else GFail
  }

  def classifyValidateResult(x: (Int, String, String)): VResult = {
    def f(y: String) = x._2.split('\n').last.contains(y)
    def g(y: String) = x._3.split('\n').filterNot(_.substring(0, 12) == "MEASURE_TIME").head.contains(y)
    def h(y: String) = x._2.split('\n').head.contains(y)

    if(f("Validation failed.")) VFail
    else if(f("Validation succeeded.")) VSuccess
    else if(h("Set to admitted.")) VAdmitted
    else if(f("Set to fail.")) VAssertionFail
    else if(g("Fatal error: exception Failure") &&
      (g("Not_Supported") || g("is not supported for now."))) VNotSupported
    // else if(f("llvm-obj/bindings/ocaml/llvm/llvm_ocaml.c:1388: llvm_instr_get_opcode: Assertion `o <= LLVMLandingPad' failed."))
    //   "Ocaml Binding Fail"
    else
      VUnknown
  }

  def get_opt_name(triple_base: String): String = {
    TimeChecker.runWithClock("get_opt_name") {
      //http://docs.scala-lang.org/tutorials/FAQ/stream-view-iterator
      try {
        val tmp = scala.io.Source.fromFile(triple_base + ".hint.json").getLines.take(4)
        val last_line = tmp.toList.last
        //use option monad instead
        if(last_line.split(":").size != 2) throw new Exception
        val str = last_line.split(":")(1)
        //really want to use just -2... personal library?
        str.substring(2, str.size-2)
      }
      catch {
        case e:Throwable =>
          "opt name parse error!!"
      }
    }
  }
}










class TestRunner(
  val crellvm_logics: CrellvmLogics,
  val option_map: Map[Symbol, String]) {

  import java.util.concurrent.atomic._
  import java.util.concurrent.atomic.AtomicLong
  import java.util.concurrent.atomic.AtomicReference
  import java.util.concurrent._

  val process_strategy = option_map.get('p).getOrElse("d")
  val num_threads = option_map.get('j).getOrElse("24").toInt
  val verbose = option_map.contains('verbose)

  def print_verbose(x: String) =
    if(verbose) Mutex.synchronized { println(s"[[THREAD#${Thread.currentThread().getId()}]]: ${x}") }

  object Mutex
  sealed abstract class Job
  case class GQJob(val ll_base: String) extends Job
  case class VQJob(val triple_base: String) extends Job
  case object NoJob extends Job
  case object Terminate extends Job

  sealed abstract class JobResult {
    val base_name: String
    val time: Double
    // val classifiedResult: String

    // f"size: ${fileSize}%20s" +
    //padTo(' ', 20) PASSES type checking!!!!!
    override def toString = base_name + DELIMITER + time
    //sometimes base_name is too long and padding cannot care it.
    //there should be spaces between them
  } //without val, it is private

  object JobResult {
    def columnNames = "baseName" + DELIMITER + "time"
  }

  class GQJobResult(
    val base_name: String,
    val fileSize: Long,
    val time: Double,
    //val wallTimes: (Double, Double, Double),
    //val userSysTimes: (Double, Double, Double),
    val unitedTimes: (Double, Double, Double, Double, Double, Double, Double, Double),
    val generated: Int,
    val classifiedResult: CrellvmLogics.GResult
  ) extends JobResult {
    override def toString: String = {
      super.toString + DELIMITER +
      unitedTimes._1 + DELIMITER +
      unitedTimes._2 + DELIMITER +
      unitedTimes._3 + DELIMITER +
      unitedTimes._4 + DELIMITER +
      unitedTimes._5 + DELIMITER +
      unitedTimes._6 + DELIMITER +
      unitedTimes._7 + DELIMITER +
      unitedTimes._8 + DELIMITER +
      fileSize + DELIMITER +
      generated + DELIMITER +
      classifiedResult
    }
  }

  object GQJobResult {
    def columnNames =
      JobResult.columnNames + DELIMITER +
    "wallTime-CombineInstructions" + DELIMITER +
    "wallTime-GVN" + DELIMITER +
    "wallTime-SROA" + DELIMITER +
    "wallTime-LICM" + DELIMITER +
    "userSysTime-CombineInstructions" + DELIMITER +
    "userSysTime-GVN" + DELIMITER +
    "userSysTime-SROA" + DELIMITER +
    "userSysTime-LICM" + DELIMITER +
    "fileSize" + DELIMITER +
    "generated" + DELIMITER +
    "classifiedResult"
  }

  class VQJobResult(
    val base_name: String,
    val fileSize: (Long, Long, Long),
    val time: Double,
    val optName: String,
    val classifiedResult: CrellvmLogics.VResult,
    val timeData: List[(Double, Double)]
  ) extends JobResult {
    override def toString: String = {
      super.toString + DELIMITER +
      fileSize._1 + DELIMITER +
      fileSize._2 + DELIMITER +
      fileSize._3 + DELIMITER +
      optName + DELIMITER +
      classifiedResult +
      timeData.
        map(x => DELIMITER + x._1 + DELIMITER + x._2).
        foldLeft("")((s, i) => s + i)
    }
  }

  object VQJobResult {
    def columnNames = {
      JobResult.columnNames + DELIMITER +
      "srcSize" + DELIMITER +
      "tgtSize" + DELIMITER +
      "hintSize" + DELIMITER +
      "optName" + DELIMITER +
      "classifiedResult" + DELIMITER +
      "start" + DELIMITER +
      "start-accumulated" + DELIMITER +
      "read-done" + DELIMITER +
      "read-done-accumulated" + DELIMITER +
      "insert-nop-done" + DELIMITER +
      "insert-nop-done-accumulated" + DELIMITER +
      "convert-hint-done" + DELIMITER +
      "convert-hint-done-accumulated" + DELIMITER +
      "validation-done" + DELIMITER +
      "validation-done-accumulated" + DELIMITER
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
      val (gres, unitedTimes) = crellvm_logics.generate(ll_base)
      val tri_bases = CrellvmLogics.get_triple_bases(ll_base)
      tri_bases.foreach(VQ.offer(_))
      Mutex.synchronized { VQ_current_total += tri_bases.size }
      val t1 = System.currentTimeMillis
      new GQJobResult(ll_base, fileSize, (t1 - t0)/1000.0, unitedTimes, tri_bases.size, gres)
    }
  }

  def processVQ(triple_base: String): VQJobResult = {
    TimeChecker.runWithClock("processVQ") {
      val t0 = System.currentTimeMillis
      val fileSize = ((new File(triple_base + ".src.bc")).length,
        new File(triple_base + ".tgt.bc").length,
        new File(triple_base + ".hint.json").length)
      val optName = CrellvmLogics.get_opt_name(triple_base)
      val (vres, timeData) = crellvm_logics.validate(triple_base)
      val t1 = System.currentTimeMillis
      new VQJobResult(triple_base, fileSize, (t1 - t0)/1000.0, optName, vres, timeData)
    }
  }

  class MyThread extends Thread {
    override def run {
      @tailrec
      def runner(): Unit = {
        printProgressIfIntervalPassed
        fetch_next_job match {
          case GQJob(ll_base) =>
            print_verbose("processGQ start -------> " + ll_base)
            val res = processGQ(ll_base)
            print_verbose("processGQ end   <------- " + ll_base)
            Mutex.synchronized { GQR += res }
            runner
          case VQJob(triple_base) =>
            print_verbose("processVQ start =======> " + triple_base)
            val res = processVQ(triple_base)
            print_verbose("processVQ end   <======= " + triple_base)
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
      CrellvmLogics.get_ll_bases(crellvm_logics.output_result_dir)
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
    printProgress
  }

  //TODO separate object
  var last_printed: Long = 0

  def printProgress = {
    if(!verbose) (1 to 7) foreach { _ => goPreviousLine }
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

  def printProgressIfIntervalPassed = Mutex.synchronized {
    TimeChecker.runWithClock("print_progress") {
      val t0 = System.currentTimeMillis()
      if(t0 - last_printed > 250) {
        last_printed = t0
        printProgress
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
    val table: Map[CrellvmLogics.GResult, Int] =
      new scala.collection.immutable.HashMap[CrellvmLogics.GResult, Int]().
        withDefaultValue(0)
    val table_filled = GQR.foldLeft(table){(s, i) =>
      s.updated(i.classifiedResult, s(i.classifiedResult) + 1)
    }
    row_to_string("All Generation")(table_filled)
  }

  def VQR_to_matrix = {
    val VblankRow = new scala.collection.immutable.HashMap[CrellvmLogics.VResult, Int]() +
    ((CrellvmLogics.VSuccess, 0)) +
    ((CrellvmLogics.VFail, 0)) +
    ((CrellvmLogics.VNotSupported, 0)) +
    ((CrellvmLogics.VAdmitted, 0)) +
    ((CrellvmLogics.VAssertionFail, 0)) +
    ((CrellvmLogics.VUnknown, 0))

    val table: Map[String, Map[CrellvmLogics.VResult, Int]] =
      new scala.collection.immutable.HashMap[String, Map[CrellvmLogics.VResult, Int]]().
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
    val VblankRow = new scala.collection.immutable.HashMap[CrellvmLogics.VResult, Int]() +
    ((CrellvmLogics.VSuccess, 0)) +
    ((CrellvmLogics.VFail, 0)) +
    ((CrellvmLogics.VNotSupported, 0)) +
    ((CrellvmLogics.VAdmitted, 0)) +
    ((CrellvmLogics.VAssertionFail, 0)) +
    ((CrellvmLogics.VUnknown, 0))

    // val table: Map[CrellvmLogics.VResult, Int] =
    //   new scala.collection.immutable.HashMap[CrellvmLogics.VResult, Int]().
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
  def GQR_to_list: String = {
    if(crellvm_logics.report_nosuccess) {
      GQJobResult.columnNames + "\n" +
      GQR.foldLeft("")((s, i) => s +
        (if(i.classifiedResult == CrellvmLogics.GSuccess) i.toString + "\n"
        else "")
      )
    }
    else {
      GQJobResult.columnNames + "\n" +
      GQR.foldLeft("")((s, i) => s + i.toString + "\n")
    }
  }
  // GQR.sortBy(_.classifiedResult.toString).toList.
  // foldRight("")((i, s) => s + i.toString + "\n")
  //without toList, it causes stack overflow
  //same foldRight name but implementation changes

  def VQR_to_list: String = {
    if(crellvm_logics.report_nosuccess) {
      VQJobResult.columnNames + "\n" +
      VQR.foldLeft(StringBuilder.newBuilder)((sbldr, i) =>
        if(i.classifiedResult == CrellvmLogics.VSuccess)
          sbldr.append(i.toString).append("\n")
        else
          sbldr
      ).toString
    }
    else {
      VQJobResult.columnNames + "\n" +
      VQR.foldLeft(StringBuilder.newBuilder)((sbldr, i) =>
        sbldr.append(i.toString).append("\n")
      ).toString()
    }
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
    "d": Default. Try with without "-d" option, and then if validation has not
         succeeded, re-run with "-d" option. Its result will be redirected to
         ".debug_result" file. It does not run llvm-dis.
    "s": Slow. Always run with "-d" option
    Default value is "d".

-j, --jobs <int>:
    Set number of threads.
    Default value is 24.

--verbose:
    Set verbose mode. It may be useful for debugging the script.

--noresult:
    Do not write ".result" files.
    Currently, it prohibits both generation and validation results.

--report-nosuccess:
    Skip GSuccess/VSuccess cases on report.generate/report.validate.
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
        case ("--verbose") :: tail =>
          nextOption(map ++ Map('verbose -> ""), tail)
        case ("--noresult") :: tail =>
          nextOption(map ++ Map('noresult -> ""), tail)
        case ("--report-nosuccess") :: tail =>
          nextOption(map ++ Map('report_nosuccess -> ""), tail)
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
    val crellvm_logics = new CrellvmLogics(option_map)

    crellvm_logics.copy_input_dir
    println(crellvm_logics.simplberry_path)
    println(crellvm_logics.output_result_dir)
    println ; println ; println(string_with_bar())
    println("Start Script")
    crellvm_logics.simplberry_path match {
      case Some(spth) =>
        crellvm_logics.compile(spth)
        println("Compile Done")
      case None =>
        println("No simplberry path specified. Not compiling")
    }
    // crellvm_logics.copy_executable
    /*
     I added copy_executable for the following use case:
     Start test, abort, and then restart the test in the same dir.
     However I think that may not occur frequently...
     Also that may require separation of crellvm_logics from TestRunner
     It may make code more dirty than clean.

     If Separation is done well, use case may be:
     1. given input_dir => copy it into test_dir
     2. given test_dir => do test
     3. given test_dir => show statistics
     It may require recording all the options(process_strategy) in some file,
     so that it can be transfered from 1 to 2.
     It will make it much more tedious...
     */

    val runner = new TestRunner(crellvm_logics, option_map)
    for(i <- 1 to 12) println
    val t0 = System.currentTimeMillis()
    runner.run
    println(TimeChecker.getData.mapValues(x =>
      format_double(x.toDouble / runner.num_threads / 1000)))
    val t1 = System.currentTimeMillis()
    println((t1-t0).toDouble / 1000)
    for(i <- 1 to 8) println
    println("Test Done")
    println(runner.GQ_total + " " + runner.VQ_current_total)

    //TODO separate report writing
    //it is more fault tolerant
    //also we may want to see report during main script is going
    def get_summary_txt =
      runner.GQR_to_row + "\n\n" + string_with_bar() + "\n" +
      runner.VQR_to_row + "\n\n" + string_with_bar() + "\n" +
      runner.VQR_to_matrix

    val (summary_txt, summary_txt_time) = TimeChecker.runWithClockSingle(get_summary_txt)
    println(s"Calculating report.summary took ${summary_txt_time}")
    val (_, summary_txt_write_time) = TimeChecker.runWithClockSingle(write_to_file(summary_txt, new File(crellvm_logics.output_result_dir + "/report.summary")))
    println(s"Writing report.summary took ${summary_txt_write_time}")

    val (generate_txt, generate_txt_time) = TimeChecker.runWithClockSingle(runner.GQR_to_list)
    println(s"Calculating report.generate took ${generate_txt_time}")
    val (_, generate_txt_write_time) = TimeChecker.runWithClockSingle(write_to_file(generate_txt, new File(crellvm_logics.output_result_dir + "/report.generate")))
    println(s"Writing report.generate took ${generate_txt_write_time}")

    val (validate_txt, validate_txt_time) = TimeChecker.runWithClockSingle(runner.VQR_to_list)
    println(s"Calculating report.validate took ${validate_txt_time}")
    val (_, validate_txt_write_time) = TimeChecker.runWithClockSingle(write_to_file(validate_txt, new File(crellvm_logics.output_result_dir + "/report.validate")))
    println(s"Writing report.validate took ${validate_txt_write_time}")

    // ("ag application " + crellvm_logics.output_result_dir).!
    println("End Script")
  }
}









val mb = 1024*1024
val runtime = Runtime.getRuntime
println("** Used Memory:  " + (runtime.totalMemory - runtime.freeMemory) / mb)
println("** Free Memory:  " + runtime.freeMemory / mb)
println("** Total Memory: " + runtime.totalMemory / mb)
println("** Max Memory:   " + runtime.maxMemory / mb)
for(_ <- 1 to 10) println

val t0 = System.currentTimeMillis()
MainScript.main(args)
val t1 = System.currentTimeMillis()
println((t1-t0).toDouble / 1000)
