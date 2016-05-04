package com.github.skozlov.turing.math01

import com.github.skozlov.turing.Tape
import org.scalatest.{Matchers, FlatSpec}
import com.github.skozlov.turing.build.Dsl._

class IncrementTest extends FlatSpec with Matchers{
	"Increment" should "return 1 for 0" in {
		val tape = new Tape("01":_*)
		tape(Increment)
		tape.caretIndex shouldBe 0
		tape.cells shouldBe cellStatesFromString("011")
	}

	it should "return 2 for 1" in {
		val tape = new Tape("011":_*)
		tape(Increment)
		tape.caretIndex shouldBe 0
		tape.cells shouldBe cellStatesFromString("0111")
	}
}