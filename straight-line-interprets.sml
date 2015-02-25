type id = string
datatype binop = Plus | Minus | Times | Div
datatype stm = CompoundStm of stm * stm
             | AssignStm of id * exp
             | PrintStm of exp list
and exp = IdExp of id
        | NumExp of int
        | OpExp of exp * binop * exp
        | EseqExp of stm * exp

(* maxargs returns the maximum number of argument of any print statement
within any subexpression of a given statement. *)
fun maxargs_old (PrintStm s) = List.length s
  | maxargs_old (AssignStm(_, exp)) = maxargs_exp exp
  | maxargs_old (CompoundStm(stm1, stm2)) = Int.max(maxargs_old stm1, maxargs_old stm2)
and maxargs_exp (EseqExp(stm, exp)) = Int.max(maxargs_old stm, maxargs_exp exp) (* mutual recursive by and *)
  | maxargs_exp (OpExp(exp1, _, exp2)) =
    Int.max(maxargs_exp exp1, maxargs_exp exp2)
  | maxargs_exp (_) = 0

(* maxargs returns the maximum number of argument of any print statement
within any subexpression of a given statement. *)
fun maxargs (PrintStm s) = List.length s
  | maxargs (AssignStm(_, (EseqExp(stm, exp)))) = maxargs stm
  | maxargs (AssignStm(_, _)) = 0
  | maxargs (CompoundStm(stm1, stm2)) = Int.max(maxargs stm1, maxargs stm2)

(* test maxargs *)
val prog1 =
    CompoundStm(AssignStm("a",OpExp(NumExp 5, Plus, NumExp 3)),
      CompoundStm(AssignStm("b",
        EseqExp(PrintStm[IdExp"a",OpExp(IdExp"a", Minus, NumExp 1)],
          OpExp(NumExp 10, Times, IdExp"a"))),
      PrintStm[IdExp "b"]))
