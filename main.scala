object Compiler {
	def main (args : Array[String]) {
    println ("=" * 80)
    
    import java.io.File
    for (f <- new File ("./input/ch03").listFiles.toList.sortWith ((f1, f2) => f1.getName < f2.getName);
         if (f.getName.endsWith (".A68"))) {
      test (MyParsers.start, f.getPath)
      println ("=" * 80)
    }
  }
}