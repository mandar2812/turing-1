package com.github.skozlov.turing

object Direction extends Enumeration {
	type Direction = Value

	val L, R = Value

	val Left, <= = L
	val Right, -> = R
}