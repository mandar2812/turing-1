package com.github.skozlov.turing.math00

import com.github.skozlov.turing.Tape
import org.scalatest.{Matchers, FlatSpec}
import com.github.skozlov.turing.build.Dsl._

class DecrementTest extends FlatSpec with Matchers {
	"Decrement" should "return 1 for 2" in {
		val tape = new Tape("011":_*)
		tape(Decrement)
		tape.caretIndex shouldBe 0
		tape.cells shouldBe cellStatesFromString("01")
	}

	it should "return 0 for 1" in {
		val tape = new Tape("01":_*)
		tape(Decrement)
		tape.caretIndex shouldBe 0
		tape.cells shouldBe cellStatesFromString("0")
	}

	it should "return 0 for 0" in {
		val tape = new Tape("0":_*)
		tape(Decrement)
		tape.caretIndex shouldBe 0
		tape.cells shouldBe cellStatesFromString("0")
	}
}