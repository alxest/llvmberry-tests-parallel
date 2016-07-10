import sys.process._
import java.io.File

object DiffScript {
  def main(args: Array[String]): Unit = {
    val rgx = """(\w)+(\s)""".r
    val _x = "/opt/devel/youngju.song/simplberry/simplberry-tests/test_result.llvm_regression_tests.13/report.detail"
    val _y = "/opt/devel/youngju.song/simplberry/simplberry-tests/test_result.llvm_regression_tests.17/report.detail"
    val x = scala.io.Source.fromFile(_x).mkString
    val y = scala.io.Source.fromFile(_y).mkString
    //http://stackoverflow.com/questions/14469958/how-to-split-sentence-into-words-separated-by-multiple-spaces

    def f = ((x: String) => x.split("\n").map(_.split("""\s+""")).filter(_.size == 7))
  //   def g = ((x: Array[Array[String]]) => x.
  //     filter{t =>
  //       try {
  //         Integer.parseInt(t.last)
  //         false // generated
  //       } catch {
  //         case _: Throwable =>
  //           true //opt_name
  //       }
  //       // true
  //     }.map(i => (i(0), i(6)))
  // )
    val xunknown = x.split("\n").indexWhere(_.contains("VUnknown"))
    val yunknown = y.split("\n").indexWhere(_.contains("VUnknown"))
    val xunknown_end = x.split("\n").indexWhere(_.contains("---------------"), xunknown + 1)
    val yunknown_end = y.split("\n").indexWhere(_.contains("---------------"), yunknown + 1)

    //after slicing, it only remains VQJobResult already
    val x_ = f(x).slice(xunknown, xunknown_end).map(i => (i(0).split('/').tail.mkString("/"), i(6)))
    val y_ = f(y).slice(yunknown, yunknown_end).map(i => (i(0).split('/').tail.mkString("/"), i(6)))
    println("Size Diff ----------------> " + x_.size + " " + y_.size)
    // println((x_.toSet - y_.toSet).size)
    // how to diff more elegant??
    val x_y = x_.filterNot(i => y_.contains(i))
    val y_x = y_.filterNot(i => x_.contains(i))
    println("------------------------------------------------------")
    x_y.foreach(println(_))
    println("------------------------------------------------------")
    y_x.foreach(println(_))
    println(x_(0))

    println(xunknown + " " + xunknown_end)
    println(yunknown + " " + yunknown_end)
    // x_.foreach{i =>
    //   i.foreach(j => println(j))
    //   println(i.size)}
    // y_.foreach{i =>
    //   i.foreach(j => println(j))
    //   println(i.size)}
    // println(x_.size + " " + y_.size)

    // x_(0).foreach{println(_)}
    // x_.last.foreach{println(_)}
  }
}

DiffScript.main(args)
