package com.github.skozlov.turing.build

import com.github.skozlov.turing.CellState._
import com.github.skozlov.turing.Implicits
import com.github.skozlov.turing.Direction.Direction
import com.github.skozlov.turing.build.ProgramBuilder.{State, Command}
import com.github.skozlov.turing.State.{Id => StateId}

trait Dsl extends Implicits{
	implicit val commandFromNextStateId: StateId => Command = Command(None, None, _)

	implicit val modifyingCommandFromCellNewState: CellState => Command.Incomplete.Modifying =
		Command.Incomplete.Modifying

	implicit val movingCommandFromMovement: Direction => Command.Incomplete.Moving = Command.Incomplete.Moving

	implicit def stateFromIdAndCommand(params: (StateId, Command)): State = {
		val (id, command) = params
		State(id, command, command)
	}

	implicit def stateFromIdAndIncompleteCommand(params: (StateId, Command.Incomplete)): State = {
		val (id, command) = params
		stateFromIdAndCommand(id, command ~ id)
	}

	implicit def stateFromIdAndCommands(params: (StateId, ((CellState, Command), (CellState, Command)))): State = {
		val (id, ((cellState1, command1), (cellState2, command2))) = params
		require(cellState1 != cellState2, s"Cell state `$cellState1` is duplicated")
		val (zeroCommand, oneCommand) = if(cellState1 == Zero) (command1, command2) else (command2, command1)
		State(id, zeroCommand = zeroCommand, oneCommand = oneCommand)
	}

	implicit def stateFromIdAndCommandAndIncompleteCommand(
                              params: (StateId, ((CellState, Command), (CellState, Command.Incomplete)))): State = {

		val (id, (cellStateAndCommand1, (cellState2, command2))) = params
		stateFromIdAndCommands(id -> (cellStateAndCommand1, cellState2 -> command2 ~ id))
	}

	implicit def stateFromIdAndIncompleteCommandAndCommand(
                              params: (StateId, ((CellState, Command.Incomplete), (CellState, Command)))): State = {

		val (id, (cellStateAndCommand1, cellStateAndCommand2)) = params
		stateFromIdAndCommandAndIncompleteCommand(id -> (cellStateAndCommand2, cellStateAndCommand1))
	}

	implicit def stateFromIdAndIncompleteCommands(
                 params: (StateId, ((CellState, Command.Incomplete), (CellState, Command.Incomplete)))): State = {

		val (id, ((cellState1, command1), (cellState2, command2))) = params
		stateFromIdAndCommands(id -> (cellState1 -> command1 ~ id, cellState2 -> command2 ~ id))
	}
}

object Dsl extends Dsl