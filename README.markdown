Implementations of Kahn process networks
----------------------------------------

Using the OCaml programming language. (Requires version >= 3.12 ?)

lib.ml contains the common interface for all implementations.

The following implementations will eventually be available :
* Thread-based (using system threads blocked by the OCaml runtime)
* With Unix forks and pipes
* Sequential implementations using continuation-based control flow
  * with a concurrency monad (cf. the functional pearl paper)
  * with coroutines
* Using network sockets and client/server architecture
  * local thread communicating over sockets on localhost
  * over the network !

They're all done except the very last one.

Note: the sequential implementations might block indefinitely on a process
executing a non-terminating and seemingly atomic (from the POV of the
concurrency handling code) action; you have to write code that cooperates
well! Other solutions, which use the OS's preemptive multitasking facilities,
do not have this flaw, but might be slower.
