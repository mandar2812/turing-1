package com.github.skozlov.turing

import com.github.skozlov.turing.CellState._

trait Implicits{
	implicit def cellStatesFromString(source: String): List[CellState] = source.toCharArray map {
		case '0' => Zero
		case '1' => One
		case _ => throw new IllegalArgumentException("Only `0` and `1` digits can be used")
	} toList
}

object Implicits extends Implicits