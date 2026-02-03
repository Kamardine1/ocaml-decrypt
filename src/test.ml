(*
Auteur :
Mattéo Raffin
Kamardine Mirghane Mohamed
*)


(* Fichiers de test portant sur diéfférente fonction de main.ml et Utils.ml *)

#use "src/main.ml";;



(* =================================== PREMIERE PARTIE =================================== *)


(* Vérifie si la fusion des fuites s'effectue bien *)
let app_merge = merge_app(get_name_list(),ref([]));;
nth(app_merge,1)

(* Récupère la deuxième fuite dans notre liste de toute les fuites (générée avec
merge_app()) *)
nth(merge_app(get_name_list(),ref([])),1);;
(* Regarde la longueur de la liste des fuites, étant donné que nous avons 3 fuites 
différente, cela renvoie donc 3 *)
len(merge_app(get_name_list(),ref([])));;





(* =================================== DEUXIEME PARTIE =================================== *)


(* Regarde la première occurence du login dans la liste data*)
let data = [{login = "111eee"; pwd = "jecrOKilbN0h0pZMweML7mootaDShOJ9ecf+YZri/VE="};{login = "111ddd"; pwd = "7oj5oFSjDryncf+GP/lNhtiieg+Vi01siqqDBdW/7CU="};{login = "kmirghan"; pwd = "Nadj11."};{login = "111ccc"; pwd = "WO2wnynSVFjKRY2CaKF7hwgXW4/lL09DQ7MbBV7NTpY="};{login = "111bbb"; pwd = "zsMwjuQ/8LaiXC/Ql3/ejoglL6lZgCYXqZ+DFrJKZTk="};{login = "111aaa"; pwd = "7JQurguTQaqzwEHeVdJT7vQgbdXrKkmb9PqkuxDuYf8="};{login = "222ccc"; pwd = "rmQkVaFziIG01hneqb4IvdwaYJYEEPwvP3MSRmYFdAk="};{login = "222bbb"; pwd = "jecrOKilbN0h0pZMweML7mootaDShOJ9ecf+YZri/VE="};{login = "222aaa"; pwd = "hzcsFgHVk9IRMjfpOHwYN8/VGv9cARyhU7rRU27aFuI="};{login = "333ddd"; pwd = "OICi0ZVeR/ucEvNdV2hmoNZ7ISSuBeeTNdek2j6r4Ws="};{login = "111aaa"; pwd = "7JQurguTQaqzwEHeVdJT7vQgbdXrKkmb9PqkuxDuYf8="};{login = "kmirghan"; pwd = "Nadj111."};{login = "333ccc"; pwd = "ohfdqJxZKHCMczNxLWayb8YRPoUZF0gDDkbXSPj2mK0="};{login = "333bbb"; pwd = "/Mk1iVxsi640SVVn2WJXfM1zsOejZi83uxxVlTzQch4="};{login = "333aaa"; pwd = "EWTKwQo8Ehqzchuz0WB5N3P7+Rt49w9/QXAE6l4aWzk="};{login = "111aaa"; pwd = "7JQurguTQaqzwAHeVdJT7vQgbdXrKkm29PqkuxDuYf8="};{login = "444eee"; pwd = "jecrOKilbN0h0pZMweML7mootaDShOJ9ecf+YZri/VE="};{login = "444ddd"; pwd = "6bPV3x1G2i7woXruSJhTlRgeNszIcq10LvLCeYT4CmM="};{login = "444ccc"; pwd = "J2PkgJZKxznvINk8woXhSaJFkrIhIVFV1xlZ/iEtTkA="};{login = "444bbb"; pwd = "qUwjenEkMxSRdFnXvPVsFUaP8ZtVLdhreR7qPAB7CDk="};{login = "444aaa"; pwd = "51AGKIHGDLmFycQE8E6Fs4dGcHkavouIUxy8VvfRd4c="}]
get_pos_log("111aaa",data,0)
(* Meme chose, mais étant donné que get_pos_log() supprime les données parcourues,
on a donc la deuxième occurence renvoyé en repartant de 0 dans la nouvelle liste data*)
let data2 = Stdlib.snd(get_pos_log("111aaa",data,0))
get_pos_log("111aaa",data2,0)


(* Récupère la liste des positions des occurences des logins *)
get_pos_log_list("111aaa",data,ref([]));;
get_pos_log_list("kmirghan",data,ref([]))
check_login_across_breachs_aux("kmirghan",ref([]),"test",data);;





(* =================================== TROISIEME PARTIE =================================== *)


let app_merge = merge_app(get_name_list(),ref([]))
let p_login = "gdorrosg"
(* Récupère la liste des couples (log,pwd) de la n-ième application (ici la 1ere) *)
Stdlib.snd(nth(app_merge,0))
(* Récupère la liste des positions des occurences du login initialisé précédement *)
get_pos_log_list(p_login,Stdlib.snd(nth(app_merge,0)),ref([]))
(* Récupère la liste des applications et mot de passe où le login a été trouvé pour 
respectivement la première application puis la deuxième de app_merge qui est une liste 
contenant toutes les données des fuites *)
check_login_across_breachs_aux("kmirghan",ref([]),Stdlib.fst(nth(app_merge,0)),Stdlib.snd(nth(app_merge,0)))
check_login_across_breachs_aux("kmirghan",ref([]),Stdlib.fst(nth(app_merge,1)),Stdlib.snd(nth(app_merge,1)))

(* Regarde si un login existe et donc quels sont ses mots de passe
Tout d'abord en précisant le paramètre app_merge utilisé dans des fonctions
Puis seulement le login pour vérifier que cela fonctionne *)
check_login_across_breachs(p_login,app_merge);;
launch_clab("kmirghan");;
launch_clab("gdorrosg");;





(* =================================== TROISIEME PARTIE =================================== *)


time merge_app (get_name_list(),ref([]))
let app_merge = merge_app(get_name_list(),ref([]))
let data3 = Stdlib.snd(List.hd(app_merge))
  (* 
  [{login = "444aaa"; pwd = "51AGKIHGDLmFycQE8E6Fs4dGcHkavouIUxy8VvfRd4c="};{login = "444bbb"; pwd = "qUwjenEkMxSRdFnXvPVsFUaP8ZtVLdhreR7qPAB7CDk="};{login = "444ccc"; pwd = "J2PkgJZKxznvINk8woXhSaJFkrIhIVFV1xlZ/iEtTkA="};{login = "444ddd"; pwd = "6bPV3x1G2i7woXruSJhTlRgeNszIcq10LvLCeYT4CmM="};{login = "444eee"; pwd = "jecrOKilbN0h0pZMweML7mootaDShOJ9ecf+YZri/VE="};{login = "111aaa"; pwd = "7JQurguTQaqzwAHeVdJT7vQgbdXrKkm29PqkuxDuYf8="};{login = "333aaa"; pwd = "EWTKwQo8Ehqzchuz0WB5N3P7+Rt49w9/QXAE6l4aWzk="};{login = "333bbb"; pwd = "/Mk1iVxsi640SVVn2WJXfM1zsOejZi83uxxVlTzQch4="};{login = "333ccc"; pwd = "ohfdqJxZKHCMczNxLWayb8YRPoUZF0gDDkbXSPj2mK0="};{login = "kmirghan"; pwd = "Nadj111."};{login = "111aaa"; pwd = "7JQurguTQaqzwEHeVdJT7vQgbdXrKkmb9PqkuxDuYf8="};{login = "333ddd"; pwd = "OICi0ZVeR/ucEvNdV2hmoNZ7ISSuBeeTNdek2j6r4Ws="};{login = "222aaa"; pwd = "hzcsFgHVk9IRMjfpOHwYN8/VGv9cARyhU7rRU27aFuI="};{login = "222bbb"; pwd = "jecrOKilbN0h0pZMweML7mootaDShOJ9ecf+YZri/VE="};{login = "222ccc"; pwd = "rmQkVaFziIG01hneqb4IvdwaYJYEEPwvP3MSRmYFdAk="};{login = "111aaa"; pwd = "7JQurguTQaqzwEHeVdJT7vQgbdXrKkmb9PqkuxDuYf8="};{login = "111bbb"; pwd = "zsMwjuQ/8LaiXC/Ql3/ejoglL6lZgCYXqZ+DFrJKZTk="};{login = "111ccc"; pwd = "WO2wnynSVFjKRY2CaKF7hwgXW4/lL09DQ7MbBV7NTpY="};{login = "kmirghan"; pwd = "Nadj111."};{login = "111ddd"; pwd = "7oj5oFSjDryncf+GP/lNhtiieg+Vi01siqqDBdW/7CU="};{login = "111eee"; pwd = "jecrOKilbN0h0pZMweML7mootaDShOJ9ecf+YZri/VE="}];;
  *)
get_pos_pwd("Nadj111.",data3,0);;
let data4 = Stdlib.snd(get_pos_pwd("Nadj111.",data3,0))
get_pos_pwd("Nadj111.",data4,0);;

get_pos_pwd_list("Nadj111.",data3,ref([]))

check_pwd_across_breachs_aux("Nadj111.",ref([]),["Test"],data3);;

check_pwd_across_breachs("Xe4J8XLJbuvbQtS6TUI6l9dnmgju2LZq5VzFKyohx+A=",app_merge)
launch_cpab("Xe4J8XLJbuvbQtS6TUI6l9dnmgju2LZq5VzFKyohx+A=")





(* =================================== QUATRIEME PARTIE =================================== *)


let result = time launch_mpwal ("french_passwords_top20000.txt");;

(*
time launch_mpwal ("french_passwords_top20000.txt");;
Execution time: 57.215666s
*)

result

len(launch_mpwal("french_passwords_top20000.txt"))

len(result)

let l_couple = get_couple_app_log("french_passwords_top20000.txt")
len(l_couple)



(* =================================== PARTIE FINALE =================================== *)


(*
vous devez arriver à craquer certains mots de passe et
extraire une liste de triplets (application web,login,mot de passe craqué)
*)

let tuple = List.hd(List.tl(List.tl(List.tl(List.tl(List.tl(List.tl(List.tl(List.tl(List.tl(List.tl(List.tl(List.tl(List.tl(List.tl(List.tl(List.tl(List.tl(List.tl(List.tl(List.tl(List.tl(List.tl(List.tl(List.tl(List.tl(result))))))))))))))))))))))))))
let l_app_log : (string * string) list = Stdlib.snd(List.hd(Stdlib.snd(a)))



(extract_crack_tuple(result));;
result
nth(result,1)
let elm : 'a ref = ref (List.hd(result))
let tmp : 'a list ref = ref (List.tl(result))
List.hd(!tmp)
tmp := List.tl(!tmp)
List.tl(result)
Stdlib.fst(List.hd(result))
(Stdlib.snd(List.hd(Stdlib.snd(!elm))))
(Stdlib.snd(List.hd(Stdlib.snd(List.hd(result)))))
(nth(result,1))
Stdlib.snd(List.hd(Stdlib.snd(nth(result,1))))
Stdlib.snd(List.hd(Stdlib.snd((nth(result,1)))))
Stdlib.snd(List.hd(Stdlib.snd(List.hd(result))))
(
  Stdlib.fst(List.hd(Stdlib.snd(List.hd(Stdlib.snd(List.hd(!tmp)))))), (* Application*)
  Stdlib.snd(List.hd(Stdlib.snd(List.hd(Stdlib.snd(List.hd(!tmp)))))),  (* Login *)
  Stdlib.fst(List.hd(result)) (* Mot de passe craqué *)
)

