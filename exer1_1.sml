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
datatype 'a tree2 = LEAF | TREE2 of 'a tree2 * (key * 'a) * 'a tree2
fun insert2(key,n,LEAF) = TREE2(LEAF,(key,n),LEAF)
  | insert2(key,n,TREE2(l,(k,a),r)) =
    if key<k
    then TREE2(insert2(key,n,l),(k,a),r)
    else if key>k
    then TREE2(l,(k,a),insert2(key,n,r))
    else TREE2(l,(key,n),r)

(* TODO use exception or others. Using 0 limits lookup's type to: *)
(* val lookup = fn : key * int tree2 -> int *)
fun lookup(key,LEAF) = 0 
  | lookup(key,TREE2(l,(k,a),r)) =
    if key<k
    then lookup(key,l)
    else if key>k
    then lookup(key,r)
    else a

(* TODO better unit test? *)
val rt = LEAF
val tr1 = insert2("b",2,insert2("a",1,rt))
val it = lookup("b",tr1)                 
