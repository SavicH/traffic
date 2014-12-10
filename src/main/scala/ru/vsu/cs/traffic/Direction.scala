package ru.vsu.cs.traffic

sealed trait Direction

object Direction extends {
  case object LEFT extends Direction
  case object RIGHT extends Direction
  case object FORWARD extends Direction
  case object BACK extends Direction
}
