(* Left lean red black tree *)
type key = string
datatype color = Red | Black               
datatype rbtree = LEAF | TREE of rbtree * key * color * rbtree

(* search a key in rbtree is same as ordinary binary tree *)
fun lookup (key,LEAF) = false
  | lookup (key,TREE(l,k,c,r)) =
    if (key<k) then lookup(key,l)
    else if (key>k) then lookup(key,r)
    else true

(* insert a key in rbtree: key * rbtree -> rbtree *)
fun insert (key,LEAF) = TREE(LEAF,key,Red,LEAF)
  | insert (key,TREE(l,k,c,r)) =
    if key < k then TREE(insert(key,l),k,c,r)
    else if key > k then TREE(l,k,c,insert(key,r))
    else TREE(l,k,c,r)

             
fun reBalance (TREE(TREE(ll,lk,Black,lr),k,c,TREE(rl,rk,Black,rr))) = (* do nothing *)
  TREE(TREE(ll,lk,Black,lr),k,c,TREE(rl,rk,Black,rr))
  | reBalance (TREE(TREE(ll,lk, = (* rotate left *)


  
  


