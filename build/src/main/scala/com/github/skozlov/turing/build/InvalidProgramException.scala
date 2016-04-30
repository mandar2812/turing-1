package com.github.skozlov.turing.build

class InvalidProgramException(message: String = null, cause: Option[Throwable] = None) extends RuntimeException(message){
	cause foreach initCause
}