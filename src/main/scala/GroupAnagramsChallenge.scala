import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

object GroupAnagramsChallenge {
  def main(args: Array[String]): Unit = {
    val case1: ListBuffer[String] = ListBuffer("pot", "cat", "top", "opt", "tac", "dog")
    val case2: ListBuffer[String] = ListBuffer("str")
    val case3: ListBuffer[String] = ListBuffer("")

    println("["+Solution.formatOutput(Solution.groupAnagrams(case1, 0, ListBuffer[ListBuffer[String]]()))+"]")
    println("["+Solution.formatOutput(Solution.groupAnagrams(case2, 0, ListBuffer[ListBuffer[String]]()))+"]")
    println("["+Solution.formatOutput(Solution.groupAnagrams(case3, 0, ListBuffer[ListBuffer[String]]()))+"]")
  }
}

object Solution {

  @tailrec
  def groupAnagrams(inputArray: ListBuffer[String], index: Int, resultArray: ListBuffer[ListBuffer[String]]): ListBuffer[ListBuffer[String]] = {
    var found: Boolean = false

    if (inputArray.length > index) {
      if (resultArray.length == 0D) {
        resultArray += ListBuffer[String](inputArray.apply(index))
        groupAnagrams(inputArray, index + 1, resultArray)
      } else {
        resultArray.iterator.takeWhile(_ => !found).foreach(anagramArray => {
          if (inputArray.apply(index).sorted == anagramArray.head.sorted) {
            anagramArray += inputArray.apply(index)
            found = true
          }
        })

        if (!found) {
          resultArray += ListBuffer(inputArray.apply(index))
        }

        groupAnagrams(inputArray, index+1, resultArray)
      }
    } else {
      resultArray
    }
  }

  def formatOutput(list: ListBuffer[ListBuffer[String]]): String = {
    var finalFormat: String = ""
    var isFirst: Boolean = true

    list.foreach(x => {
      if (!isFirst) {
        finalFormat += ","
      }

      finalFormat += "[\"" + x.mkString("\",\"") + "\"]"
      isFirst = false
    })

    finalFormat
  }
}