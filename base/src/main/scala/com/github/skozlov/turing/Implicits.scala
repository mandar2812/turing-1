package com.github.skozlov.turing

import com.github.skozlov.turing.CellState._
import com.github.skozlov.turing.Tape.{Finite, LeftBounded}

trait Implicits{
	implicit def cellStatesFromString(source: String): List[CellState] = source.toCharArray map {
		case '0' => Zero
		case '1' => One
		case _ => throw new IllegalArgumentException("Only `0` and `1` digits can be used")
	} toList

	implicit def tapeFromString(source: String): Tape = new Tape(cellStatesFromString(source):_*)

	implicit def leftBoundedTapeFromString(source: String): Tape.LeftBounded =
		new LeftBounded(cellStatesFromString(source):_*)

	implicit def finiteTapeFromString(source: String): Tape.Finite = cellStatesFromString(source) match {
		case c :: rest => new Finite(c, rest:_*)
		case Nil => throw new IllegalArgumentException("Empty string is not allowed here")
	}

	implicit class TapeBuilder(source: String){
		lazy val tape: Tape = tapeFromString(source)

		lazy val leftBoundedTape: Tape.LeftBounded = leftBoundedTapeFromString(source)

		lazy val finiteTape: Tape.Finite = finiteTapeFromString(source)
	}
}

object Implicits extends Implicits