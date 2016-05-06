package com.github.skozlov.turing

import com.github.skozlov.turing.CellState._
import com.github.skozlov.turing.Direction._
import com.github.skozlov.turing.State.NonTerminal

import scala.collection.mutable.ArrayBuffer
import com.github.skozlov.turing.Tape._

class Tape(initData: CellState*) extends Equals{
	private val _cells: ArrayBuffer[CellState] = {
		val size: Int = Math.max(1, initData.lastIndexOf(One) + 1)
		ArrayBuffer((if(initData.isEmpty) Seq(Zero) else initData take size):_*)
	}

	private var _caretIndex: Int = 0

	def currentCell: CellState = _cells(_caretIndex)

	@throws[OutOfBoundsException]
	def apply(program: Program): Unit = apply(program.initialState)

	@throws[OutOfBoundsException]
	private def apply(state: State): Unit = state match {
		case NonTerminal(_, commands) =>
			val command = commands(currentCell)
			apply(command)
			apply(command.nextState)
		case _ =>
	}

	@throws[OutOfBoundsException]
	private def apply(command: Command): Unit = {
		command.cellNewState foreach {_cells(_caretIndex) = _}
		command.movement foreach moveCaret
	}

	@throws[OutOfBoundsException]
	protected def moveCaret(direction: Direction): Unit = {
		if(_caretIndex == _cells.size - 1){
			if(direction == Direction.Right){
				_cells append Zero
			}
			else if(currentCell == Zero){
				_cells.remove(_caretIndex)
			}
		}
		_caretIndex = if(direction == Direction.Left) _caretIndex - 1 else _caretIndex + 1
	}

	def caretIndex: Int = _caretIndex

	def cells: Seq[CellState] = _cells

	override def toString: String = {
		val prefix: String = if(_caretIndex == 0) "" else _cells.take(_caretIndex - 1).mkString
		val suffix: String = _cells.takeRight(_cells.size - _caretIndex).mkString
		s"$prefix>$suffix"
	}

	override def hashCode(): Int = (cells, _caretIndex).hashCode()

	override def equals(obj: scala.Any): Boolean = obj match {
		case that: Tape =>
			(that != null) && (that canEqual this) &&
				(this._cells == that._cells) && (this._caretIndex == that._caretIndex)
		case _ => false
	}

	override def canEqual(that: Any): Boolean = that != null && that.isInstanceOf[Tape]
}

object Tape{
	class LeftBounded(initData: CellState*) extends Tape(initData:_*) {
		@throws[OutOfBoundsException.Left]
		override protected def moveCaret(direction: Direction): Unit = {
			if(direction == Direction.Left && caretIndex == 0){
				throw new OutOfBoundsException.Left
			} else super.moveCaret(direction)
		}
	}

	class Finite(cell1: CellState, otherCells: CellState*) extends LeftBounded(cell1 +: otherCells:_*) {
		@throws[OutOfBoundsException]
		override protected def moveCaret(direction: Direction): Unit = {
			if(direction == Direction.Right && caretIndex == otherCells.size){
				throw new OutOfBoundsException.Right
			} else super.moveCaret(direction)
		}
	}

	class OutOfBoundsException(message: String = null, cause: Option[Throwable] = None)
		extends RuntimeException(message){

		cause foreach initCause
	}

	object OutOfBoundsException{
		class Left(message: String = "Cannot move left", cause: Option[Throwable] = None)
			extends OutOfBoundsException(message, cause)

		class Right(message: String = "Cannot move right", cause: Option[Throwable] = None)
			extends OutOfBoundsException(message, cause)
	}
}