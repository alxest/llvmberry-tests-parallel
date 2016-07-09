import sys.process._
import java.io.File

object abc extends App {
  def exec(cmd: String): (Int, String, String) = {
    val o = new StringBuilder
    val e = new StringBuilder
    val myProcessLogger =
      ProcessLogger(o.append(_).append("\n"), e.append(_).append("\n"))
    val retCode =
      stringSeqToProcess(Seq("/bin/sh", "-c", cmd)) ! myProcessLogger
    (retCode, o.result(), e.result())
  }


  def write_to_file(contents: String, file: File): Unit = {
    import java.io.PrintWriter
    val p = new PrintWriter(file)
    p.write(contents)
    p.close
  }

  val cmd =
    "ruby /opt/devel/youngju.song/rabbitmq/my_script/scala/src/main/scala/simple.rb"
  val t0 = System.currentTimeMillis()
  if(args.size == 0) {
    stringSeqToProcess(
      Seq("/bin/sh",
        "-c",
        s"${cmd} > b")).!
      //47.217
//       [youngju.song@cn04]/opt/devel/youngju.song/rabbitmq/my_script/scala/src/main/scala% scala -J-Xmx4g test.scala
// 46.631
// [youngju.song@cn04]/opt/devel/youngju.song/rabbitmq/my_script/scala/src/main/scala% scala -J-Xmx4g test.scala
// Exception in thread "Thread-1" java.lang.OutOfMemoryError: Java heap space
//         at java.util.Arrays.copyOf(Arrays.java:3332)
//         at java.lang.AbstractStringBuilder.expandCapacity(AbstractStringBuilder.java:137)
//         at java.lang.AbstractStringBuilder.ensureCapacityInternal(AbstractStringBuilder.java:121)
//         at java.lang.AbstractStringBuilder.append(AbstractStringBuilder.java:569)
//         at java.lang.StringBuffer.append(StringBuffer.java:369)
//         at java.io.BufferedReader.readLine(BufferedReader.java:370)
//         at java.io.BufferedReader.readLine(BufferedReader.java:389)
//         at scala.sys.process.BasicIO$$anonfun$processFully$1$$anonfun$apply$6.apply(BasicIO.scala:165)
//         at scala.sys.process.BasicIO$$anonfun$processFully$1$$anonfun$apply$6.apply(BasicIO.scala:165)
//         at scala.sys.process.BasicIO$.readFully$1(BasicIO.scala:178)
//         at scala.sys.process.BasicIO$.processLinesFully(BasicIO.scala:188)
//         at scala.sys.process.BasicIO$$anonfun$processFully$1.apply(BasicIO.scala:165)
//         at scala.sys.process.BasicIO$$anonfun$processFully$1.apply(BasicIO.scala:163)
//         at scala.sys.process.ProcessBuilderImpl$Simple$$anonfun$3.apply$mcV$sp(ProcessBuilderImpl.scala:74)
//         at scala.sys.process.ProcessImpl$Spawn$$anon$1.run(ProcessImpl.scala:23)
// 35.036
// [youngju.song@cn04]/opt/devel/youngju.song/rabbitmq/my_script/scala/src/main/scala% scala -J-Xmx4g test.scala 
// ^C/opt/devel/youngju.song/rabbitmq/my_script/scala/src/main/scala/simple.rb:1:in `write': Interrupt
//         from /opt/devel/youngju.song/rabbitmq/my_script/scala/src/main/scala/simple.rb:1:in `print'
//         from /opt/devel/youngju.song/rabbitmq/my_script/scala/src/main/scala/simple.rb:1:in `block in <main>'
//         from /opt/devel/youngju.song/rabbitmq/my_script/scala/src/main/scala/simple.rb:1:in `times'
//         from /opt/devel/youngju.song/rabbitmq/my_script/scala/src/main/scala/simple.rb:1:in `<main>'
// [youngju.song@cn04]/opt/devel/youngju.song/rabbitmq/my_script/scala/src/main/scala% 
// [youngju.song@cn04]/opt/devel/youngju.song/rabbitmq/my_script/scala/src/main/scala% scala -J-Xmx16g test.scala                                                                                                
// 46.948
// [youngju.song@cn04]/opt/devel/youngju.song/rabbitmq/my_script/scala/src/main/scala% scala -J-Xmx4g test.scala                                                                                                 
// ^C/opt/devel/youngju.song/rabbitmq/my_script/scala/src/main/scala/simple.rb:1:in `write': Interrupt
//         from /opt/devel/youngju.song/rabbitmq/my_script/scala/src/main/scala/simple.rb:1:in `print'
//         from /opt/devel/youngju.song/rabbitmq/my_script/scala/src/main/scala/simple.rb:1:in `block in <main>'
//         from /opt/devel/youngju.song/rabbitmq/my_script/scala/src/main/scala/simple.rb:1:in `times'
//         from /opt/devel/youngju.song/rabbitmq/my_script/scala/src/main/scala/simple.rb:1:in `<main>'
// 0.86
// [youngju.song@cn04]/opt/devel/youngju.song/rabbitmq/my_script/scala/src/main/scala% 
// [youngju.song@cn04]/opt/devel/youngju.song/rabbitmq/my_script/scala/src/main/scala% scala -J-Xmx16g test.scala                                                                                                
// 46.727
// [youngju.song@cn04]/opt/devel/youngju.song/rabbitmq/my_script/scala/src/main/scala% scala -J-Xmx16g test.scala
// 46.148
// [youngju.song@cn04]/opt/devel/youngju.song/rabbitmq/my_script/scala/src/main/scala% scala -J-Xmx16g test.scala

  }
  else if(args.size == 1) {
    val res = exec(cmd)
    write_to_file(res._2,
      new File("/opt/devel/youngju.song/rabbitmq/my_script/scala/src/main/scala"
        + "/a"))
    //[error] (Thread-20) java.lang.OutOfMemoryError: Java heap space
//java.lang.OutOfMemoryError: Java heap space
    // scala -J-Xmx16g test.scala
//     [youngju.song@cn04]/opt/devel/youngju.song/rabbitmq/my_script/scala/src/main/scala% scala -J-Xmx16g test.scala abcabc
// 56.136
// [youngju.song@cn04]/opt/devel/youngju.song/rabbitmq/my_script/scala/src/main/scala% scala -J-Xmx16g test.scala abcabc
// 60.427
// [youngju.song@cn04]/opt/devel/youngju.song/rabbitmq/my_script/scala/src/main/scala% scala -J-Xmx16g test.scala abcabc
// 60.676
// [youngju.song@cn04]/opt/devel/youngju.song/rabbitmq/my_script/scala/src/main/scala% scala -J-Xmx16g test.scala abcabc
// 62.249

  }

  else if(args.size == 2) {
    // fileToProcess(new File("b"))
    // stringSeqToProcess(Seq("/bin/sh", "-c", "ls")).!
    println(cmd)
    type a = ProcessBuilder
    stringSeqToProcess(Seq("/bin/sh", "-c", cmd)).
      #>(new File("/opt/devel/youngju.song/rabbitmq/my_script/scala/src/main/scala" + "/c")).!

//     [youngju.song@cn04]/opt/devel/youngju.song/rabbitmq/my_script/scala/src/main/scala% scala test.scala abc def
// ruby /opt/devel/youngju.song/rabbitmq/my_script/scala/src/main/scala/simple.rb
// 48.313

  }

  // "a" match {
  //   case "a" => println("gggggggggggggggggggggGG")
  //   case "a" => println("gggggggggggggggggggggGG")
  //   case "a" | "b" => println("gggggggggggggggggggggGG")
  // }
  val t1 = System.currentTimeMillis()
  println((t1 - t0) * 1.0 / 1000)
}

object regexRunner extends App {
  // val rgx = """test_result\.(\w)\.""".r
  val rgx = """test_result[.](.*)[.](\d+)""".r
  def r(x: String) = x match {
    // case rgx(_*) => println("Bound")
    case rgx(dir, num) => println("Bound : " + s"${dir} ||||||||||| ${num}")
    case _ => println("No match")
  }
  r("""test_result.""")
  r("""test_result..""")
  r("""test_result.tt""" )
  r("""test_result.tt.""" )
  r("""test_result.llvm_regression_test.""" )
  r("""test_result.llvm_regression_test.0""" )
  r("""test_result.llvm_regression_test.22""" )
  r("""test_result.llvm_regression_test.2cucu2""" )
  r("""test_result.llvm_regression_test.cucu22""" )
  r("""test_result.llvm_regression_test.22cucu""" )
}

regexRunner.main(args)
