(*
Auteur :
Mattéo Raffin
Kamardine Mirghane Mohamed
*)

let swap(t, i, j: 'a array * int * int) : unit =
  let temp : 'a = t.(i) in
  ( t.(i) <- t.(j);
  t.(j) <- temp );;

let tri_insertion(t : 'a array) : 'a array =
  let len : int = Array.length t and res : 'a array = t in
    for i = 1 to len -1 do
      let valeur : 'a = res.(i)
      and j : int ref = ref (i-1) in
        while !j>=0 && res.(!j) > valeur do
        swap(res,!j, !j+1);
        j:=!j -1
        done;
    done;
    res;;


(* Permet de faire un tri sur un tableau en entrée sans réecrire sur celui-ci, afin 
de pouvoir continuer de manipuler la version non trié si besoin dans le code qui 
appelle cette fonction *)
let tri_arr(t : 'a array) : 'a array =
  let t_aux : 'a array ref = ref t in
    ( t_aux := tri_insertion(!t_aux);
      !t_aux );;