package org.derekwyatt

import sbt._
import sbt.Keys._
import scala.collection.mutable.ListBuffer
import scala.util.matching.Regex
import java.util.regex.Matcher.quoteReplacement

object BookCompilePlugin extends Plugin {
  val latexDirectory      = SettingKey[File]("latex-directory")
  val graphvizDirectory   = SettingKey[File]("graphviz-directory")
  val bookTargetDirectory = SettingKey[File]("book-target-directory")
  val testThenCompile     = TaskKey[inc.Analysis]("test-then-compile")
  val testOnlyThenCompile = TaskKey[inc.Analysis]("test-only-then-compile")
  val latexSources        = TaskKey[Seq[File]]("latex-sources")
  val texSources          = TaskKey[Seq[File]]("tex-sources")
  val dotSources          = TaskKey[Seq[File]]("dot-sources")
  val neatoSources        = TaskKey[Seq[File]]("neato-sources")

  val BookConfig = config("book") extend(Compile, Test)

  val srcfileIncludesRE  = """INCLUDE_SOURCE_FILE\{([^}]+)\}""".r
  val sectionIncludesRE  = """INCLUDE_SOURCE_FILE_SECTION\{([^}]+),([^}]+)\}""".r
  val graphicsOuputDirRE = "GRAPHICS_OUTPUT_DIR".r
  val texPreprocessDirRE = "TEX_PREPROCESS_DIR".r
  val vimFoldRE          = """//[\{\}]\d""".r
  val vimModelineRE      = """//\s+vim:[^\n]*\n""".r
  val fileSectionRE      = """ *(//|#)\s*FILE_SECTION_(BEGIN|END)\{[^}]*\}\n""".r

  lazy val bookConfigSettings: Seq[Setting[_]] = inConfig(BookConfig)(
    Defaults.configSettings ++ Seq(
    latexDirectory <<= sourceDirectory / "latex",
    graphvizDirectory <<= sourceDirectory / "graphviz",
    bookTargetDirectory <<= target / "book",
    latexSources <<= latexDirectory map { d =>
      IO.listFiles(d, FileFilter.globFilter("*.latex")).toSeq
    },
    texSources <<= latexDirectory map { d =>
      IO.listFiles(d, FileFilter.globFilter("*.tex")).toSeq
    },
    dotSources <<= graphvizDirectory map { d =>
      IO.listFiles(d, FileFilter.globFilter("*.dot")).toSeq
    },
    neatoSources <<= graphvizDirectory map { d =>
      IO.listFiles(d, FileFilter.globFilter("*.neato")).toSeq
    },
    compile <<= bookCompile dependsOn (compile in Compile),
    test <<= (test in Test),
    testThenCompile <<= bookCompile dependsOn (test in Test),
    testOnly <<= (testOnly in Test) dependsOn compile
    //testOnlyThenCompile <<= bookCompile dependsOn (testOnly in Test)
  )) ++ Seq(
    watchSources <++= (latexSources in BookConfig),
    watchSources <++= (texSources in BookConfig),
    watchSources <++= (dotSources in BookConfig),
    watchSources <++= (neatoSources in BookConfig)
  )

  def processGraphViz(infile: File, outdir: File, log: ProcessLogger) = {
    val srcfile = infile.getAbsoluteFile
    val ext = srcfile.getName.split('.').last // where is basename() and extname()?
    val outfile = ("\\." + ext + "$").r.replaceAllIn(srcfile.getName, ".pdf")
    val targetFile = outdir / outfile
    if (!targetFile.exists || targetFile.lastModified < srcfile.lastModified) {
      log.info("Building %s in %s".format(outfile, outdir))
      val exitCode = Process(List(ext, "-Tpdf", "-o", targetFile.toString, srcfile.toString)) ! log
      if (exitCode != 0) sys.error("Error building graph file " + srcfile)
      true
    }
    else
      false
  }

  implicit def string2Regexable(s: String) = new Regexable(s)
  class Regexable2(data: String, re: Regex) {
    def withString(replacement: String) =
      new Regexable(re.replaceAllIn(data, quoteReplacement(replacement)))
  }
  class Regexable(val data: String) {
    def replaceAllOf(re: Regex) = new Regexable2(data, re)
  }
  implicit def regexable2String(re: Regexable) = re.data

  class FoldableRegex(toSearchIn: String) {
    def over(re: Regex) = new {
      def fold(f: (String, String) => String) = re.findAllIn(toSearchIn).foldLeft(toSearchIn)(f)
    }
  }
  implicit def string2FoldableRegex(s: String) = new FoldableRegex(s)

  def pullSection(data: Seq[String], section: String): Seq[String] = {
    val i = data.iterator
    var pull = false
    val begin = ("""^\s*(//|#) FILE_SECTION_BEGIN\{""" + section + """\}""").r
    val end = ("""^\s*(//|#) FILE_SECTION_END\{""" + section + """\}""").r
    val buffer = ListBuffer.empty[String]
    while (i.hasNext) {
      val s = i.next
      if (begin.findFirstIn(s).isDefined) pull = true
      else if (end.findFirstIn(s).isDefined) pull = false
      else if (pull) buffer += s
    }
    buffer.toSeq
  }

  def pullSrcFileFromGit(location: String): Seq[String] = {
    val Split = ("^([^:]+):(.*)").r
    val Split(branch, file) = location
    val branchlist = (Process(command = List("git", "branch"), cwd = new File("src")) !!).split("\n")
    val currentBranch = branchlist.filter { _ startsWith "*" }.map { _ stripPrefix "* " }.headOption
    val data = if (currentBranch.isDefined && currentBranch.get == branch) {
      val f = new File("src/" + file)
      if (f.exists) IO.read(f)
      else ""
    } else {
      (Process(command = List("git", "show", location), cwd = new File("src")) !!)
    }
    if (data.isEmpty)
      Vector(file + " NOT FOUND")
    else
      data.split('\n')
  }

  def makeLatexSubstitutions(latexSrcFile: File, targetDir: File, outFilename: String): File = {
    val outfile = targetDir / outFilename
    val convertedData = IO.read(latexSrcFile) over srcfileIncludesRE fold { (data, matched) =>
      val srcfileIncludesRE(fileName) = matched
      val toInsert = pullSrcFileFromGit(fileName) mkString("\n")
      (("""INCLUDE_SOURCE_FILE\{""" + fileName + """\}""").r).replaceAllIn(data, quoteReplacement(toInsert))
    } over sectionIncludesRE fold { (data, matched) =>
      val sectionIncludesRE(fileName, section) = matched
      val toInsert = pullSrcFileFromGit(fileName)
      (("""INCLUDE_SOURCE_FILE_SECTION\{""" + fileName + """,\s*""" + section + """\}""").r).replaceAllIn(data,
        quoteReplacement(pullSection(toInsert, section).mkString("\n")))
    } replaceAllOf graphicsOuputDirRE withString targetDir.toString replaceAllOf
        texPreprocessDirRE withString targetDir.toString replaceAllOf
        vimFoldRE withString "" replaceAllOf
        vimModelineRE withString "" replaceAllOf
        fileSectionRE withString ""
    IO.write(outfile, convertedData)
    outfile
  }

  def processTexFile(infile: File, outdir: File, log: ProcessLogger) = {
    val targetFile = outdir / infile.getName
    if (!targetFile.exists || targetFile.lastModified < infile.lastModified) {
      makeLatexSubstitutions(infile, outdir, infile.getName)
      true
    }
    else
      false
  }

  def processBookFile(needToBuild: Boolean, bookFile: File, outdir: File, log: ProcessLogger) = {
    val outfile = ("\\.latex".r).replaceAllIn(bookFile.getName, ".pdf")
    val outfileNoExt = ("\\.latex".r).replaceAllIn(bookFile.getName, "")
    val tempfile = makeLatexSubstitutions(bookFile, outdir, outfileNoExt + ".tmp.latex")
    val targetFile = outdir / outfile
    if (needToBuild || !targetFile.exists || targetFile.lastModified < bookFile.lastModified) {
      log.info("Building %s in %s".format(outfile, outdir))
      val exitCode = Process(List("pdflatex",
                                  "-output-directory=" + outdir,
                                  "-jobname=" + outfileNoExt,
                                  "-halt-on-error",
                                  tempfile.toString)) ! log
      if (exitCode != 0) sys.error("Error building PDF file " + bookFile)
    }
  }

  def bookCompile = (latexSources, texSources, dotSources, neatoSources, bookTargetDirectory, streams) map {
    (latexFs, texFs, dotFs, neatoFs, targ, s) => {
      targ.mkdirs
      val logger = new ProcessLogger {
        def info(o: => String): Unit = s.log.info(o)
        def error(e: => String): Unit = s.log.error(e)
        def buffer[T](f: => T): T = f
      }
      val needToBuild = ((dotFs map   { f => processGraphViz(f, targ, logger) }) ++
                         (neatoFs map { f => processGraphViz(f, targ, logger) })) ++
                         (texFs map   { f => processTexFile(f, targ, logger) }) exists (_ == true)
      latexFs foreach { f => processBookFile(needToBuild, f, targ, logger) }
      sbt.inc.Analysis.Empty
    }
  }
}
