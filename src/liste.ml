(*
Auteur :
Mattéo Raffin
Kamardine Mirghane Mohamed
*)


let createlist() : 'a list = [];;

let isempty(li : 'a list) : bool = (li = []) ;;


let rec len(li : 'a list) : int =
  if isempty(li) 
    then 0
  else 1+len(List.tl li)
;;


let invliste(li : 'a list) : 'a list = 
  let rec invaux(lu,l_res : 'a list * 'a list) : 'a list =
    if isempty(lu)
      then l_res
    else invaux(List.tl(lu), List.hd(lu) :: l_res)
  in
invaux(li,[]);
;;


let concat(l1,l2 : 'a list * 'a list) : 'a list =
  let rec concat_aux(la,lb,lr : 'a list * 'a list * 'a list) : 'a list = 
    if isempty(la)
      then
        if isempty(lb)
          then lr
        else
          concat_aux([], List.tl(lb), List.hd(lb) :: lr)
    else
      concat_aux(List.tl(la), lb, List.hd(la) :: lr)
  in
  invliste(concat_aux(l1,l2,[]))
;;




let nth(li, pos : 'a list * int) : 'a =
  let rec nthaux(li,pos : 'a list * int) : 'a =
    if li=[]
      then failwith("Error nth : index out of bounds")
    else 
      if pos = 0 
        then List.hd li
      else nthaux(List.tl li, pos-1)
  in
    if pos<0 
      then failwith("Error nth : index must be positive")
    else nthaux(li, pos)
;;



let first(li : 'a list) : 'a = nth(li,0) ;;

let lst(li : 'a list) : 'a = nth(li, len(li)-1) ;;

let add_fst(li, e : 'a list * 'a) : 'a list = e::li ;;

let add_lst(li, e : 'a list * 'a) : 'a list = concat(li, [e]);;

let rem_fst(li : 'a list) : 'a list = List.tl li;;


let rem_lst(li : 'a list) : 'a list = 
    let rec rem_aux(li,lu : 'a list * 'a list) : 'a list =
      if List.tl(li) = []
        then lu
      else 
        rem_aux(List.tl(li),List.hd(li) :: lu);
    in
      invliste(rem_aux(li,[]));
;;



let rem_nth(li, pos : 'a list * int) : ' list =
  let ind: int ref = ref pos
  and lu : 'a list ref = ref []
  and lv : 'a list ref = ref li in
    ( 
      while !lv <> [] 
      do
        if !ind <> 0 
          then ( lu := (List.hd !lv) :: !lu;
                lv := List.tl !lv;
                ind := !ind-1 )
        else ( lv := List.tl !lv;
              ind := !ind-1)
        done;
        invliste(!lu)
    )
;;




