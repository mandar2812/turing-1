package com.github.skozlov.turing

import com.github.skozlov.turing.build.Dsl._
import com.github.skozlov.turing.build.ProgramBuilder
import Direction._
import CellState._

package object math00{
	val Decrement: Program = ProgramBuilder(
		"q1" -> R~"q2",
		"q2" -> (`1` -> R.c, `0` -> L~"q3"),
		"q3" -> (`0` -> "q0".c, `1` -> `0`~L~"q4"),
		"q4" -> (`1` -> L.c, `0` -> "q0".c)
	).toProgram
}