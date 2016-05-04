package com.github.skozlov.turing

import com.github.skozlov.turing.build.ProgramBuilder
import CellState._
import Direction._
import com.github.skozlov.turing.build.Dsl._

package object math01{
	val Decrement: Program = ProgramBuilder(
		"q1" -> (`0` -> R.c, `1` -> R~"q2"),
		"q2" -> (`0` -> L~"q5", `1` -> R~"q3"),
		"q3" -> (`1` -> R.c, `0` -> L~"q4"),
		"q4" -> `0`~L~"q5",
		"q5" -> (`1` -> L.c, `0` -> "q0".c)
	).toProgram
}