package com.hw

import com.hw.MixAlphabetAndNumberWithModular

object FuntionTest {
  def main(args: Array[String]): Unit = {
    val testStr = """<abc>def123!@#</abc>"""
    assert(MixAlphabetAndNumberWithModular.getValidChar(testStr, true).equals("""abcdef123abc"""))
    assert(MixAlphabetAndNumberWithModular.getValidChar(testStr, false).equals("""def123"""))
    assert(MixAlphabetAndNumberWithModular.maxedResult("""def123""").equals("""d1e2f3"""))
    assert(MixAlphabetAndNumberWithModular.maxedResult("""AAAbbb""").equals("""AAAbbb"""))
    assert(MixAlphabetAndNumberWithModular.maxedResult("""aAbBb""").equals("""AaBbb"""))
    assert(MixAlphabetAndNumberWithModular.maxedResult("""987aAbBb""").equals("""A7a8B9bb"""))
    assert(MixAlphabetAndNumberWithModular.maxedResult("""987aAbBb""").equals("""A7a8B9bb"""))
    assert("123456789".substring(0, 3).equals("123"))
    assert("123456789".substring(3).equals("456789"))
    println("success")
  }
}
