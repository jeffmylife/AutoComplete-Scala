import scala.io.Source
import scala.collection.mutable.ListBuffer

object AutoComplete extends App {

  val datapath = "/Users/jeffreylemoine/Projects/AutoComplete/data/"
  val cities_fname = "cities.txt"
  val wiki_fname = "wiktionary.txt"

  println("Choose Dataset:\n(1) Cities or (2) Wiktionary")
  val choice = scala.io.StdIn.readLine()

  val fileName = datapath + (if (choice=="1") cities_fname else wiki_fname)
  println(fileName)
  println("Weird choice, but okay!")
  println("\n\nNow enter your query so I can autocomplete it (for example, type \"New\")")
  print("\nQuery:\t")

  val prefix: String = scala.io.StdIn.readLine()

  type Term = (Long, String)
  def formatRow(row: String) : Term = {
    val (weight, query) = row.split("\t") match {case Array(a,b) => (a, b)}
    (weight.trim.toLong, query).asInstanceOf[Term]
  }

  def parseCSV[A](fileName: String, skip: Int, rowFormatter: String => A) : List[A] = {
    val rows = ListBuffer[A]()
    val file = Source.fromFile(fileName)
    try {
      file.getLines().drop(skip).foreach {
        line => rows += rowFormatter(line)
      }
    }
    finally{
      file.close()
    }
    rows.toList
  }

  // parsed data
  val terms:List[Term] = parseCSV[Term](fileName, 1, formatRow)

  // the actual autocompletion logic
  val matches = terms.map {term =>
    val (weight, query) = term

    // number of direct matches inside query
    val numMatches:Int = prefix.toLowerCase.r.findAllMatchIn(query.trim.toLowerCase).length

    // number of prefix matches (most important)
    val matchLength = prefix.length - ((query.trim.toLowerCase.slice(0, prefix.length) zip prefix.toLowerCase) dropWhile{case (a,b) => a==b }).length

    (weight, numMatches, matchLength, query)
  } collect {case (w, n, m, q) if n>0 => (w, n, m, q)}

  // sort by the most well-matched query, weight, then # of direct prefix-query matches
  val result = matches.sortBy(t => (-t._3, -t._1, t._2)).map(tup => tup._4)


  // Print Results
  if (result.length==0) {
    println("\nSorry, nothing close to what you typed was found. ")
  }
  else {
    print("\n"+ result.length.toString + " match(es) found.")
    if (result.length >10){
      println("Here's 10: \n")
      result.take(10).foreach(println)
    }
    else{
      println()
      result.foreach(println)
    }
  }

}
