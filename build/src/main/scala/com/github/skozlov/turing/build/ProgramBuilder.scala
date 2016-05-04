package com.github.skozlov.turing.build

import com.github.skozlov.turing.CellState._
import com.github.skozlov.turing.Direction.Direction
import com.github.skozlov.turing.State.{Id => StateId}
import com.github.skozlov.turing.build.ProgramBuilder._
import com.github.skozlov.turing.{Direction, Program, State => RealState}

case class ProgramBuilder private(private val initialStateId: StateId, private val states: Map[StateId, State]){
	require(states contains initialStateId)

	def +(state: State): ProgramBuilder = {
		require(!states.contains(state.id), s"State `${state.id}` is duplicated")
		ProgramBuilder(initialStateId, states + (state.id -> state))
	}

	@throws[InvalidProgramException]
	def toProgram: Program = {
		val realStates: Map[StateId, RealState.NonTerminal] = states map {
			case (stateId, state) =>
				(stateId,
					RealState(stateId, zeroCommand = state.zeroCommand.toReal, oneCommand = state.oneCommand.toReal))
		}
		for(
			(stateId, state) <- realStates;
			stateDescriptor = states(stateId);
			zeroNextStateId = stateDescriptor.zeroCommand.nextStateId;
			oneNextStateId = stateDescriptor.oneCommand.nextStateId
		){
			state.zeroCommand.asInstanceOf[com.github.skozlov.turing.Command.Mutable].nextState =
				realStates.getOrElse(zeroNextStateId, RealState.Terminal(zeroNextStateId))
			state.oneCommand.asInstanceOf[com.github.skozlov.turing.Command.Mutable].nextState =
				realStates.getOrElse(oneNextStateId, RealState.Terminal(oneNextStateId))
		}
		Program(initialState = realStates(initialStateId))
	}

	override def toString: String = {
		val sortedStates: List[State] = states(initialStateId) :: (states - initialStateId).values.toList
		sortedStates mkString "\n"
	}
}

object ProgramBuilder{
	def apply(initialState: State, otherStates: State*): ProgramBuilder = otherStates.toList match {
		case Nil => ProgramBuilder(initialState.id, Map(initialState.id -> initialState))
		case state2 :: rest =>ProgramBuilder(initialState, rest:_*) + state2
	}

	case class Command(cellNewState: Option[CellState] = None, movement: Option[Direction] = None, nextStateId: StateId){
		override lazy val toString: String = {
			val setCellStr = cellNewState map {_.toString + ' '} getOrElse ""
			val moveStr = movement map {_.toString} getOrElse "N"
			s"$setCellStr$moveStr $nextStateId"
		}

		def toReal: com.github.skozlov.turing.Command.Mutable = {
			val command = new com.github.skozlov.turing.Command.Mutable
			command.cellNewState = cellNewState
			command.movement = movement
			command
		}

		val c = this
	}

	object Command{
		trait Incomplete{
			val c: this.type = this

			def ~(nextStateId: StateId): Command
		}

		object Incomplete{
			implicit class Moving(movement: Direction) extends Incomplete {
				protected val cellNewState: Option[CellState] = None

				override def ~(nextStateId: StateId): Command = Command(cellNewState, Some(movement), nextStateId)
			}

			implicit class Modifying(cellNewState: CellState) extends Incomplete {
				override def ~(nextStateId: StateId): Command = Command(Some(cellNewState), None, nextStateId)

				def ~(movement: Direction): Moving = new Moving(movement){
					override protected val cellNewState: Option[CellState] = Some(Modifying.this.cellNewState)
				}

				def left(nextStateId: StateId): Command = this ~ Direction.Left ~ nextStateId

				def right(nextStateId: StateId): Command = this ~ Direction.Right ~ nextStateId

				val L: StateId => Command = left

				val R: StateId => Command = right
			}
		}
	}

	case class State(id: StateId, zeroCommand: Command, oneCommand: Command){
		override lazy val toString: String = s"$id: 0 -> $zeroCommand, 1 -> $oneCommand"

		override lazy val hashCode: Int = id.hashCode

		override def equals(obj: scala.Any): Boolean = obj match {
			case that: State => (that != null) && (that canEqual this) && (this.id == that.id)
			case _ => false
		}
	}
}