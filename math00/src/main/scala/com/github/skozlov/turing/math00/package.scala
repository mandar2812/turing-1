package com.github.skozlov.turing

import com.github.skozlov.turing.build.Dsl._
import com.github.skozlov.turing.build.ProgramBuilder
import Direction._
import CellState._

package object math00{
	/**
	 * Returns `(x-1)` for a number `x`; `0` for `0`.
	 * @example `011` -> `01`
	 * @example `01` -> `0`
	 * @example `0` -> `0`
	 */
	val Decrement: Program = ProgramBuilder(
		"q1" -> R~"q2",
		"q2" -> (`1` -> R.c, `0` -> L~"q3"),
		"q3" -> (`0` -> "q0".c, `1` -> `0`~L~"q4"),
		"q4" -> (`1` -> L.c, `0` -> "q0".c)
	).toProgram

	/**
	 * Returns the sum of 2 numbers.
	 * @example `000` -> `0`
	 * @example `0100` -> `01`
	 * @example `01010` -> `011`
	 */
	val Sum: Program = ProgramBuilder(
		"q1" -> R~"q2",
		"q2" -> (`1` -> R.c, `0` -> `1`~R~"q3"),
		"q3" -> (`1` -> R.c, `0` -> L~"q4"),
		"q4" -> `0`~L~"q5",
		"q5" -> (`1` -> L.c, `0` -> "q0".c)
	).toProgram
}