Send Expression
---------------

Expr1 ! Expr2

Sends the value of Expr2 as a message to the process specified by Expr1. The
value of Expr2 is also the return value of the expression.

Expr1 must evaluate to a pid, a registered name (atom) or a tuple (Name,Node),
where Name is an atom and Node a node name, also an atom.

 * If Expr1 evaluates to a name, but this name is not registered, a badarg
   run-time error will occur.

 * Sending a message to a pid never fails, even if the pid identifies a
   non-existing process.

 * Distributed message sending, that is if Expr1 evaluates to a tuple
   (Name,Node) (or a pid located at another node), also never fails.

