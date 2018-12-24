package com.hw

import java.util.ArrayList
import scala.io.Source

object MixAlphabetAndNumberWithModular {
  case class AlphabetBean(c: Char, isLower: Boolean) {}
  case class IndexBean(c: Char, idx: Int, isNum: Boolean) {}

  def main(args: Array[String]): Unit = {

    val (modular, urlContent, isTXT) = getinputData()

    if (modular <= 0) {
      println("modular should be over 0")
      return
    }

    // valid character만 가져옵니다.
    var remainValidChar = getValidChar(urlContent, isTXT)

    // 숫자와 영문의 shuffle data를 산출합니다.
    val mixBetweenAlphaNNumber = maxedResult(remainValidChar)

    // 몫 부분과 나머지 부분을 출력합니다.
    val modularStr = mixBetweenAlphaNNumber.substring(0, mixBetweenAlphaNNumber.size / modular * modular)
    val remainderStr = mixBetweenAlphaNNumber.substring(mixBetweenAlphaNNumber.size / modular * modular)

    println("몫 : " + modularStr)
    println("나머지 : " + remainderStr)

  }

  def maxedResult(remainValidChar: String) = {
    val patternNumber = "([^0-9]*)".r
    val patternAlphabet = "([^a-zA-Z]*)".r

    val numstr = patternNumber.replaceAllIn(remainValidChar, """""")
    val alphabet = patternAlphabet.replaceAllIn(remainValidChar, """""")

    val alphabetSorted: List[AlphabetBean] = alphabet.toList.map(i => new AlphabetBean(i.toLower, i.isLower)).sortBy(s => (s.c, s.isLower))
    val numberSorted = numstr.toList.sortBy(i => i)

    val retAlphabet = alphabetSorted.zipWithIndex.map(i => if (i._1.isLower) IndexBean(i._1.c, i._2, false) else IndexBean(i._1.c.toUpper, i._2, false))
    val retNum = numberSorted.zipWithIndex.map(i => IndexBean(i._1, i._2, true))

    val mixBetweenAlphaNNumber: String = retAlphabet.union(retNum).sortBy(s => (s.idx, s.isNum)).map(i => (i.c)).mkString("")
    mixBetweenAlphaNNumber
  }

  def getValidChar(urlContent: String, isTXT: Boolean) = {
    val patternRemove = "(<[^<>]*>)".r
    val removedHtmlTagStr = patternRemove.replaceAllIn(urlContent, """""")

    val patternRemain = "([^0-9A-Za-z]+)".r
    var remainValidChar = ""

    if (isTXT == true) {
      remainValidChar = patternRemain.replaceAllIn(urlContent, """""")
    } else {
      remainValidChar = patternRemain.replaceAllIn(removedHtmlTagStr, """""")
    }
    remainValidChar
  }

  def getinputData() = {
    val urlstr = readLine("input url > ").replaceAll(""" """, "")
    val isTXT: Boolean = readLine("input type (TXT / HTML) > ").replaceAll(""" """, "").toString().toLowerCase().equals("txt")
    val modularStr = readLine("input modular> ").replaceAll(""" """, "")
    if (modularStr.toInt.toString().equals(modularStr) == false) {
      println("출력묶음단위는 반드시 양수 이어야 합니다")
      System.exit(1)
    }

    val html = Source.fromURL(urlstr)
    val urlContent = html.mkString
    (modularStr.toInt, urlContent, isTXT)
  }
}