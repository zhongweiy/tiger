type key = string
datatype tree = LEAF | TREE of tree * key * tree
val empty = LEAF

fun insert(key,LEAF) = TREE(LEAF,key,LEAF)
  | insert(key,TREE(l,k,r)) =
    if key<k
    then TREE(insert(key,l),k,r)
    else if key>k
    then TREE(l,k,insert(key,r))
    else TREE(l,key,r)

(* return true if item is found, else false *)
fun member(key,LEAF) = false
  | member(key,TREE(l,k,r)) =
    if key<k
    then member(key,l)
    else if key>k
    then member(key,r)
    else true

(* extend the program: the mapping of keys to bindings *)
datatype 'a tree2 = LEAF2 | TREE2 of 'a tree2 * (key * 'a) * 'a tree2
fun insert2(key,n,LEAF2) = TREE2(LEAF2,(key,n),LEAF2)
  | insert2(key,n,TREE2(l,(k,a),r)) =
    if key<k
    then TREE2(insert2(key,n,l),(k,a),r)
    else if key>k
    then TREE2(l,(k,a),insert2(key,n,r))
    else TREE2(l,(key,n),r)

(* TODO use exception or others. Using 0 limits lookup's type to: *)
(* val lookup = fn : key * int tree2 -> int *)
fun lookup(key,LEAF2) = 0 
  | lookup(key,TREE2(l,(k,a),r)) =
    if key<k
    then lookup(key,l)
    else if key>k
    then lookup(key,r)
    else a

(* Print a binary tree *)
fun printBinTree(t) =
  let
      fun spaces n = if n = 0 then "" else " " ^ (spaces (n - 1))
      fun helper (LEAF, n) = print ((spaces n) ^ "Nil\n")
        | helper (TREE(l,k,r), n) = (helper (r,(n+4));
                                     print (spaces n); print k; print "\n";
                                     helper (l,(n+4)))
  in
      helper (t,0)
  end
      

(* TODO better unit test? *)
val rt = LEAF2
val tr2 = insert2("b",2,insert2("a",1,rt))
val it = lookup("b",tr2)

val tr1 = insert("b",insert("a",LEAF))               

(* TODO refer Sedgewick's Algorithms book and work out a balance tree *)
               
