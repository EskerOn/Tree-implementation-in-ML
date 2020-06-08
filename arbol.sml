datatype 'a Btree = nul | nod of 'a * 'a Btree * 'a Btree;

(*append*)
fun app(nil,l2)=l2
|   app(f::r,l2)=f::app(r,l2);

fun balance(list)=
    let
        fun putOnTree(any, nul)= nod(any,nul,nul)
        |   putOnTree(any, nod(r,i,d))= if any<=r then
                                            nod(r, putOnTree(any, i), d)
                                        else 
                                            nod(r,i,putOnTree(any, d))
        fun li2tr(nil, tre)= tre
        |   li2tr(h::rlist, tre) = li2tr(rlist, putOnTree(h, tre)) 
    in
        li2tr(list, nul)
    end;

(*Tree transversals - TAREA*)
(*
preorder RID tree->list
inorder IRD tree->list
postorder IDR tree->list
*)
(*
fun preorder(nul)=[]
|   preorder(nod(r,i,d))= app(r::preorder(i),preorder(d));

fun inorder(nul)=[]
|   inorder(nod(r,i,d))= app(inorder(i),r::inorder(d));

fun postorder(nul)=[]
|   postorder(nod(r,i,d))= app(postorder(i),app(postorder(d),[r]));

fun order(list)=inorder(balance(list));(*función que toma una lista, la introduce en un arbol y a través de la funcion inorder la devuelve ordenada de forma ascendente*)
*)

fun preorder(nul, list)=list
|   preorder(nod(r,i,d), list)= r::preorder(i, preorder(d,list));

fun inorder(nul,list)=list
|   inorder(nod(r,i,d),list)= inorder(i, r::inorder(d,list));

fun postorder(nul,list)=list
|   postorder(nod(r,i,d),list)= postorder(d,postorder(i,list))@[r];

fun inor(list)=inorder(balance(list),[]);
fun preor(list)=preorder(balance(list),[]);
fun postor(list)=postorder(balance(list),[]);

(* Ejemplo nod(1,nod(2,nod(3,nul,nul),nod(4,nul,nod(8,nul,nul))),nod(5,nod(6,nod(9,nul,nul),nul),nod(7,nul,nul)))

           1
         /    \
        2      5
       / \    / \
      3   4  6   7
          \ / 
          8 9
Preorder:[1,2,3,4,8,5,6,9,7]
Inorder:[3,2,4,8,1,9,6,5,7]
PostOrder:[3,8,4,2,9,6,7,5,1]
*)


(*
(*fun concat*)
fun preorder(nul)=[]
|   preorder(nod(r,i,d))= List.concat([r::preorder(i),preorder(d)]);
fun inorder(nul)=[]
|   inorder(nod(r,i,d))= List.concat([inorder(i),r::inorder(d)]);
fun postorder(nul)=[]
|   postorder(nod(r,i,d))= List.concat([postorder(i),List.concat([postorder(d),[r]])]);
(*"@" append operator*)
fun preorder(nul)=[]
|   preorder(nod(r,i,d))= [r]@preorder(i)@preorder(d);
fun inorder(nul)=[]
|   inorder(nod(r,i,d))= inorder(i)@[r]@inorder(d);
fun postorder(nul)=[]
|   postorder(nod(r,i,d))= postorder(i)@postorder(d)@[r]; 
*)




