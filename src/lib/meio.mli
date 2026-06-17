module Logging = Logging

val ui :
  ?prog:string * string list ->
  child_pid:int ->
  start:int64 ->
  string * int ->
  unit
