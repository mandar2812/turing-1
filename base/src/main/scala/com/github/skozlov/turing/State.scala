package com.github.skozlov.turing

import State._
import com.github.skozlov.turing.CellState._

sealed trait State{
	val id: Id
}

object State{
	type Id = String

	case class Terminal(override val id: Id = "S0") extends State with Equals {
		override val hashCode: Int = 0

		override val toString: String = id.toString

		override def equals(obj: scala.Any): Boolean = obj match {
			case that: Terminal => (that != null) && (that canEqual this)
			case _ => false
		}

		override def canEqual(that: Any): Boolean = (that != null) && that.isInstanceOf[Terminal]
	}

	val terminal = Terminal()

	class NonTerminal(override val id: Id, val zeroCommand: Command, val oneCommand: Command) extends State{
		val commands = Map(Zero -> zeroCommand, One -> oneCommand)

		override lazy val toString: String = s"$id: 0 -> $zeroCommand, 1 -> $oneCommand"
	}

	object NonTerminal{
		def unapply(obj: Any): Option[(Id, Map[CellState, Command])] = obj match {
			case that: NonTerminal => Some((that.id, that.commands))
			case _ => None
		}
	}

	def apply(id: Id, command: Command): NonTerminal = new NonTerminal(id, command, command)

	def apply(id: Id, cellState2Command: (CellState, Command)): NonTerminal =
		State(id, Map(cellState2Command)).asInstanceOf[NonTerminal]

	def apply(id: Id, zeroCommand: Command, oneCommand: Command): NonTerminal =
		new NonTerminal(id, zeroCommand = zeroCommand, oneCommand = oneCommand)

	def apply(id: Id, cellStates2Commands: Map[CellState, Command]): State = {
		if(cellStates2Commands.isEmpty){
			Terminal(id)
		} else {
			val defaultCommand: Command = Command(nextState = terminal)
			val zeroCommand = cellStates2Commands.getOrElse(Zero, defaultCommand)
			val oneCommand = cellStates2Commands.getOrElse(One, defaultCommand)
			new NonTerminal(id, zeroCommand = zeroCommand, oneCommand = oneCommand)
		}
	}
}