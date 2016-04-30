package com.github.skozlov.turing

import java.util.Objects

import com.github.skozlov.turing.CellState._
import com.github.skozlov.turing.Direction._

trait Command extends Equals{
	def cellNewState: Option[CellState]
	def movement: Option[Direction]
	def nextState: State

	override def toString: String = {
		val setCellStr = cellNewState map {_.toString + ' '} getOrElse ""
		val moveStr = movement map {_.toString} getOrElse "N"
		s"$setCellStr$moveStr ${nextState.id}"
	}

	override def equals(obj: scala.Any): Boolean = obj match {
		case that: Command =>
			(that != null) &&
				(that canEqual this) &&
				(this.cellNewState == that.cellNewState) &&
				(this.movement == that.movement) &&
				(this.nextState == that.nextState)
		case _ => false
	}

	override def hashCode(): Int = Objects.hash(cellNewState, movement,nextState)

	override def canEqual(that: Any): Boolean = (that != null) && that.isInstanceOf[Command]
}

object Command{
	def unapply(command: Command): Option[(Option[CellState], Option[Direction], State)] =
		Some((command.cellNewState, command.movement, command.nextState))

	case class Immutable(
		                    override val cellNewState: Option[CellState],
		                    override val movement: Option[Direction],
		                    override val nextState: State) extends Command

	def apply(cellNewState: Option[CellState] = None, movement: Option[Direction] = None, nextState: State): Immutable =
		Immutable(cellNewState, movement, nextState)

	class Mutable extends Command {
		private var _cellNewState: Option[CellState] = None
		private var _movement: Option[Direction] = None
		private var _nextState: Option[State] = None

		override def cellNewState: Option[CellState] = _cellNewState

		def cellNewState_=(state: Option[CellState]): Unit ={
			_cellNewState = state
		}

		override def nextState: State = _nextState.get

		def nextState_=(state: State): Unit ={
			_nextState = Some(state)
		}

		override def movement: Option[Direction] = _movement

		def movement_=(m: Option[Direction]): Unit ={
			_movement = m
		}
	}
}