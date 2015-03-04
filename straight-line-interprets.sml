type id = string
datatype binop = Plus | Minus | Times | Div
datatype stm = CompoundStm of stm * stm
             | AssignStm of id * exp
             | PrintStm of exp list
and exp = IdExp of id
        | NumExp of int
        | OpExp of exp * binop * exp
        | EseqExp of stm * exp

fun maxargs (PrintStm ([])) = 0
  | maxargs (PrintStm (n::ns)) =
    let val ec1 = List.length ns + 1
        val ec2 = maxargs_exp n
        val ec3 = maxargs(PrintStm(ns))
    in
        if ec1 > ec2 then
            if ec1 > ec3 then ec1 else ec3
        else if ec2 > ec3 then ec2 else ec3
    end
  | maxargs (CompoundStm (stm1, stm2)) = Int.max(maxargs stm1, maxargs stm2)
  | maxargs (AssignStm(_, (EseqExp(stm, exp)))) = maxargs stm
  | maxargs (AssignStm(_, _)) = 0
and maxargs_exp (EseqExp(stm,exp)) = Int.max(maxargs stm, maxargs_exp exp)
  | maxargs_exp (OpExp(exp1,_,exp2)) = Int.max(maxargs_exp exp1, maxargs_exp exp2)
  | maxargs_exp (_) = 0

(* test maxargs. *)
(* TODO add unit test *)
(* maxargs prog1 should return 2 *)
val prog1 =
    CompoundStm(AssignStm("a",OpExp(NumExp 5, Plus, NumExp 3)),
      CompoundStm(AssignStm("b",
        EseqExp(PrintStm[IdExp"a",OpExp(IdExp"a", Minus, NumExp 1)],
          OpExp(NumExp 10, Times, IdExp"a"))),
      PrintStm[IdExp "b"]))
(* maxargs prog2 should return 1 *)
val prog2 = PrintStm[IdExp"a"]
(* maxargs prog3 should return 4 *)
val prog3 = PrintStm[IdExp"a",
                     OpExp(IdExp"a", Minus, NumExp 1),
                     EseqExp(PrintStm[IdExp"a",
                                      IdExp"b",
                                      IdExp"c",
                                      IdExp"d"],IdExp("a"))]

(* maxargs prog4 should return 5 *)
val prog4 = PrintStm[IdExp"a",
                     OpExp(IdExp"a", Minus,
                           EseqExp(PrintStm[IdExp"a",
                                            IdExp"b",
                                            IdExp"c",
                                            IdExp"d",
                                            IdExp"e"],IdExp("2"))),
                     EseqExp(PrintStm[IdExp"a",
                                      IdExp"b",
                                      IdExp"c",
                                      IdExp"d"],IdExp("a"))]

(* maxargs prog5 should return 6 *)
val prog5 = PrintStm[IdExp"a",
                     OpExp(EseqExp(PrintStm[IdExp"a",
                                            IdExp"b",
                                            IdExp"c",
                                            IdExp"d",
                                            IdExp"e",
                                            IdExp"f"],IdExp("2")),
                           Minus,
                           EseqExp(PrintStm[IdExp"a",
                                            IdExp"b",
                                            IdExp"c",
                                            IdExp"d",
                                            IdExp"e"],IdExp("2"))),
                     EseqExp(PrintStm[IdExp"a",
                                      IdExp"b",
                                      IdExp"c",
                                      IdExp"d"],IdExp("a"))]

(* stm->table *)
fun interp (stm)=
  let (* id x table -> int *)
      fun lookup (id, ([])) = 0 (* TODO use null? *)
        | lookup (id, (n::ns) : (string * int) list) =
          if id = (#1 n) then (#2 n) else lookup(id, ns)

      (* stm x table -> table *)
      fun interpStm (CompoundStm (stm1, stm2), table) = interpStm(stm2, interpStm(stm1, table))
        | interpStm (AssignStm (id, exp), table) =
          let val p = interpExp(exp, table)
          in (id, (#1 p))::(#2 p) end
        | interpStm (PrintStm ([]), table) = table
        | interpStm (PrintStm (n::ns), table) = interpStm(PrintStm(ns), (#2 (interpExp(n, table))))

      (* exp x table -> int x table *)
      and interpExp (OpExp (exp1, Plus, exp2), table) =
          let val p1 = interpExp(exp1, table)
              val p2 = interpExp(exp2, (#2 p1))
          in ((#1 p1) + (#1 p2), (#2 p2)) end
        (* TODO simplify opt here *)
        | interpExp (OpExp (exp1, Minus, exp2), table) =
          let val p1 = interpExp(exp1, table)
              val p2 = interpExp(exp2, (#2 p1))
          in ((#1 p1) - (#1 p2), (#2 p2)) end
        | interpExp (OpExp (exp1, Times, exp2), table) =
          let val p1 = interpExp(exp1, table)
              val p2 = interpExp(exp2, (#2 p1))
          in ((#1 p1) * (#1 p2), (#2 p2)) end
        | interpExp (OpExp (exp1, Div, exp2), table) =
          let val p1 = interpExp(exp1, table)
              val p2 = interpExp(exp2, (#2 p1))
          in ((#1 p1) div (#1 p2), (#2 p2)) end              
        | interpExp (IdExp (id), table) = (lookup(id, table), table)
        | interpExp (NumExp (n), table) = (n, table)
        | interpExp (EseqExp (stm, exp), table) = interpExp(exp, interpStm(stm, table))
  in interpStm(stm, []) end

(* TODO unit test here: interp prog1; return val it = [("b",80),("a",8)] : (id * int) list *)

val prog6 =
    CompoundStm(AssignStm("a",OpExp(NumExp 5, Plus, NumExp 3)),
                CompoundStm(AssignStm("b",
                                      EseqExp(PrintStm[IdExp"a",OpExp(IdExp"a", Minus, NumExp 1)],
                                              OpExp(NumExp 10, Times, IdExp"a"))),
                            AssignStm("b",OpExp(NumExp 16, Div, NumExp 5))))
(* TODO unittest: interp prog6 return val it = [("b",3),("b",80),("a",8)] : (id * int) list *)
