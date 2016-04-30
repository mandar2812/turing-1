package com.github.skozlov.turing.build

import com.github.skozlov.turing.CellState._
import com.github.skozlov.turing.build.ProgramBuilder.{State, Command}
import com.github.skozlov.turing.State.{Id => StateId}

trait Dsl{
	implicit def stateFromIdAndCommand(params: (StateId, Command)): State = {
		val (id, command) = params
		State(id, command, command)
	}

	implicit def stateFromIdAndNextId(params: (StateId, StateId)): State = {
		val (id, nextId) = params
		stateFromIdAndCommand(id, commandFromNextStateId(nextId))
	}

	implicit def stateFromIdAndCommands(params: (StateId, ((CellState, Command), (CellState, Command)))): State = {
		val (id, ((cellState1, command1), (cellState2, command2))) = params
		require(cellState1 != cellState2, s"Cell state `$cellState1` is duplicated")
		val (zeroCommand, oneCommand) = if(cellState1 == Zero) (command1, command2) else (command2, command1)
		State(id, zeroCommand = zeroCommand, oneCommand = oneCommand)
	}

	implicit val commandBuilderFromNextStateId: StateId => Command.Builder = Command.Builder
	
	implicit val commandFromNextStateId: StateId => Command = Command(None, None, _)

	implicit val fullCommandFromCellState: CellState => Command.Full = Command.Full
}

object Dsl extends Dsl