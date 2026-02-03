(*
Auteur :
Mattéo Raffin
Kamardine Mirghane Mohamed
*)

#use "src/tri.ml";;
#use "src/liste.ml";;


type t_data = { login : string ; pwd : string};;



(* =================================== PREMIERE PARTIE =================================== *)


(* fonction qui verifie si un fichier est un txt *)
let is_txt_file(p_file : string) : bool = (
  (p_file.[String.length(p_file)-4]='.') &&
  (p_file.[String.length(p_file)-3]='t') &&
  (p_file.[String.length(p_file)-2]='x') && 
  (p_file.[String.length(p_file)-1]='t'))
;;


(* fonction qui regarde si une donnee est present dans une liste de donnee *)
let rec is_data_in_list(p_list,p_elt : t_data list * t_data) : bool =
  if p_list = []
    then false
  else
    if List.hd(p_list) = p_elt
      then true
    else is_data_in_list(List.tl(p_list),p_elt)
;;


(* fonction qui regarde si un elt est present dans une liste *)
let rec is_in_list(p_list,p_elt : 'a list * 'a) : bool =
  if p_list = []
    then false
  else
    if List.hd(p_list) = p_elt
    then true
    else is_in_list(List.tl(p_list),p_elt)
;;


(* fonction pour connaitre le nombre de fichiers d'une application qui a fuité donc aussi le nombre de fois qu'il a fuité *)
let number_of_file(application : string) : int =
  let dir : string array = tri_insertion(Sys.readdir("Data")) in
    let len : int = Array.length dir 
    and count : int ref = ref 0 in
      for i=0 to len-1 do (
        if dir.(i) = application^"0"^string_of_int(!count+1)^".txt" || dir.(i) = application^string_of_int(!count+1)^".txt"
        then count := !count +1 )
      done;
      !count
;;


(* inseres les fichiers de l'appli dans un tableau, en connaissant le nombre de fuites *)
let insert_n_file_in_tab(application,n : string * int) : string array =
  let tab : string array = Array.make n ("") in
    for i = 0 to n-1 do (
      if i < 10
      then tab.(i) <- (application^"0"^string_of_int(i+1)^".txt")
      else tab.(i) <- (application^string_of_int(i+1)^".txt") )
    done;
    tab
;;


(* transforme tableau en liste *)
let transform_tab_to_list(tab : 'a array) : 'a list =
  let res : 'a list ref = ref [] 
  and len : int = Array.length(tab) in
    for i = len-1 downto 0 do (
        res := tab.(i) :: !res )
    done;
    !res
;;


(* les 3 fonctions du haut permettent d'avoir une liste contenant les fichiers des fuites d'une application *)
(* fonction auxilliaire de la fonction en dessous *)
let rec recup_all_txt_aux(all_file,res : string list * string list ref) : string list =
  if all_file=[]
  then !res
  else
    if is_txt_file(List.hd(all_file))
      then (
        res := List.hd(all_file) :: recup_all_txt_aux(List.tl(all_file),res);
        !res)
    else recup_all_txt_aux(List.tl(all_file),res)
;;


(* fonction qui recupere tout les fichers .txt d'un repertoire *)
let recup_all_txt() : string list =
  let all_file : string list = transform_tab_to_list(Sys.readdir("Data")) 
  and res : string list ref = ref [] in
    recup_all_txt_aux(all_file,res)
;;



(* Génération d'une liste de nom d'application :
  get_name_list
    aux_get_name
      add_char
      verif_doublon *)

(* Permet de vérifier les doublons a condition que la liste en entrée soit trié *)
let rec verif_doublon(l,l_res : string list * string list ref) : string list =
  if l = [] then !l_res
  else (
    if List.tl(l) = [] 
      then ( l_res := List.hd(l) :: !l_res;
              verif_doublon([],l_res))
    else ( 
      if List.hd(l) = List.hd(List.tl(l))
        then ( verif_doublon(List.tl(l),l_res) )
      else  
        ( l_res := List.hd(l) :: !l_res;
          verif_doublon(List.tl(l),l_res) ) 
    )
  )
;;


(* Parcours un nom de fichier pour retirer le nombre de fuites et les .txt 
et ne garder que le nom de l'application *)
let rec add_char(app,name,j : string * string * int) : string =
  if app.[j] >= '0' && app.[j] <= '9'
    then name
  else ( add_char(app,name^Char.escaped(app.[j]),j+1) )
;;


(* Fonction auxiliaure qui renvoie la liste des noms d'applications raccourcie 
l_app ayant été trié grace a la fonction tri_arr(), verif_doublon() ne provoquera 
pas d'erreur *)
let aux_get_name(l_app : string list) : string list =
  let l_res : string list ref = ref []
  and l_tmp : string list ref = ref l_app
  and name : string ref = ref "" in
    for i=0 to len(l_app)-1 do
      let app : string = List.hd(!l_tmp) in
        ( name := "";
          name := add_char(app,!name,0);
          l_res := !name :: !l_res;
          l_tmp := List.tl(!l_tmp))
    done;
    verif_doublon(!l_res,ref [])
;;



(* Renvoie une liste avec les nom d'application seulement 
Elle renvoie les noms des applications, sans doublons, sans le nombre de fuite 
ni les .txt grace a la fonction tri_arr() *)
let get_name_list() : string list =
  let t_app : string array = tri_arr(Sys.readdir("Data"))
  and l_app : string list ref = ref [] in
    ( 
      for i=0 to Array.length(t_app)-1 do
        l_app := t_app.(i) :: !l_app
      done;
      aux_get_name(!l_app)
    )
;;



(* =================================== DEUXIEME PARTIE =================================== *)


(* fonction auxilliaire de la fonction is_login_in_lists *)
let rec is_login_in_lists_aux(p_list,p_elt,p_counter : t_data list * string * int ref) : bool =
  if p_list = []
  then
    ( if !p_counter >= 1
      then true
      else false )
  else
    if (List.hd(p_list)).login = p_elt
      then is_login_in_lists_aux(List.tl(p_list),p_elt,ref(!p_counter+1))
    else is_login_in_lists_aux(List.tl(p_list),p_elt,p_counter)
;;


(* regarde si un login est present une fois dans une liste de fuite de données *)
let is_login_in_lists(p_list,p_elt : t_data list * string) : bool =
  let counter : int ref = ref 0 in
    is_login_in_lists_aux(p_list,p_elt,counter)
;;


(* fonction qui donne le mot de passe d'un login donnee en fouillant dans une database donnee*)
let rec pwd_of(p_login,db: string * t_data list) : string =
  if db = []
    then "pwd not found"
  else
    if (List.hd(db)).login = p_login
      then (List.hd(db)).pwd
    else pwd_of(p_login,List.tl(db))
;;


(* Permet de récupérer la position de la première occurence trouvé du login tout en 
renvoyant celle ci et la liste des éléments non parcouru pour pouvoir ensuite rappeler
cette fonction sur cette fois si les éléments non parcouru *)
let rec get_pos_log(elt,app,cmpt : string * t_data list * int) : (int * t_data list) =
  if app = []
    then (-1,[])
  else (
    if elt = (List.hd(app)).login
      then (cmpt,List.tl(app))
    else get_pos_log(elt,List.tl(app),cmpt+1)
  )
;;


(* Permet de renvoyer une liste avec les positions correctes des occurences du login
ou mot de passe 
(entrée : [-1; 4; 4; 6], vrai position : [6; 11; 16]) *)
let rec correct_list(l,tot,l_res : int list * int * int list) : int list =
  if l = []
    then invliste(l_res)
  else (
    if List.hd(l) = -1 then correct_list(List.tl(l),tot,l_res)
    else correct_list(List.tl(l),List.hd(l)+tot+1,(List.hd(l)+tot)::l_res))
;;
    

(* Renvoie la liste des positions des occurences du login en ayant vérifié que ces 
positions soit cohérente.
Etant donné que get_pos_log() renvoie la liste des éléments non parcourue et qu'on se
sert de celle ci pour pouvoir atteindre la fin de la liste, les positions sont donc 
reinitialiser à chaque fois, c'est pourquoi correct_list() les corrige afin que ces 
positions soient réellement les occurences du login *)    
let rec get_pos_log_list(elt,app,l_res : string * t_data list * int list ref) : int list =
  if app = []
    then correct_list(invliste(!l_res),0,[])
  else (
    let pos_app : (int*t_data list) = get_pos_log(elt,app,0) in
      l_res := Stdlib.fst(pos_app) :: !l_res;
      get_pos_log_list(elt,Stdlib.snd(pos_app),l_res)
  )
;;



(* =================================== TROISIEME PARTIE =================================== *)


(* fonction auxilliaire de la fonction is_pwd_in_lists *)
let rec is_pwd_in_lists_aux(p_list,p_elt,p_counter : t_data list * string * int ref) : bool =
  if p_list = []
  then
    ( if !p_counter >= 1
        then true
      else false )
  else
    if (List.hd(p_list)).pwd = p_elt
      then is_pwd_in_lists_aux(List.tl(p_list),p_elt,ref(!p_counter+1))
    else is_pwd_in_lists_aux(List.tl(p_list),p_elt,p_counter)
;;


(* regarde si un pwd est present une fois dans une liste de fuite de données *)
let is_pwd_in_lists(p_list,p_elt : t_data list * string) : bool =
  let counter : int ref = ref 0 in
    is_pwd_in_lists_aux(p_list,p_elt,counter)
;;


(* fonction donnant le login d'un pwd donnee *)
let rec login_of(p_pwd,db: string * t_data list) : string =
  if db = []
    then "login not found"
  else
    if (List.hd(db)).pwd = p_pwd
      then (List.hd(db)).login
    else login_of(p_pwd,List.tl(db))
;;


(* Permet de récupérer la position de la première occurence trouvé du mot de passe tout en 
renvoyant celle ci et la liste des éléments non parcouru pour pouvoir ensuite rappeler
cette fonction sur cette fois si les éléments non parcouru *)
let rec get_pos_pwd(elt,app,cmpt : string * t_data list * int) : (int * t_data list) =
  if app = []
    then (-1,[])
  else (
    if elt = (List.hd(app)).pwd
      then (cmpt,List.tl(app))
    else get_pos_pwd(elt,List.tl(app), cmpt+1)
  )
;;


(* Renvoie la liste des positions des occurences du mot de passe en ayant vérifié que 
ces positions soit cohérente.
Etant donné que get_pos_log() renvoie la liste des éléments non parcourue et qu'on se
sert de celle ci pour pouvoir atteindre la fin de la liste, les positions sont donc 
reinitialiser à chaque fois, c'est pourquoi correct_list() les corrige afin que ces 
positions soient réellement les occurences du login *)    
let rec get_pos_pwd_list(elt,app,l_res : string * t_data list * int list ref) : int list =
  if app = []
    then correct_list(invliste(!l_res),0,[])
  else (
    let pos_app : (int*t_data list) = get_pos_pwd(elt,app,0) in
      l_res := Stdlib.fst(pos_app) :: !l_res;
      get_pos_pwd_list(elt,Stdlib.snd(pos_app),l_res)
  )
;;



(* =================================== QUATRIEME PARTIE =================================== *)


(* Fonction qui vérifie si une chaine de caractère est un mot de passe haché 
étant donné qu'un mot de passe haché fini forcément dans notre cas pas un = et
à une longueur fixe *)
let is_hashed(pwd : string) : bool = 
  (pwd.[String.length(pwd)-1]) = '=' && String.length(pwd)=44;;


(* Permet de parcourir un fichier de mot de passe et de renvoyer une liste 
contennant ces mots de passe, adaptation de la fonction read_data_from_file() 
qui nous as été fournis *)
let read_pwd_file file =
  let f = open_in file in
  let rec aux acc =
    try
      let pwd = input_line f
       in
        aux ((pwd) :: acc)
    with End_of_file ->
      close_in f;
    List.rev acc
  in
  aux []
;;


(* Prends en paramètre un fichier et renvoie la liste de son contenu. *)
let init_list_clear_pwd(file : string) : string list ref =
  ref (read_pwd_file(file))
;;


(* Fonction qui calcule le temps d'execution d'une fonction passé en paramètre *)
let time f x =
  let t = Sys.time() in
  let fx = f x in
  Printf.printf "Execution time: %fs\n" (Sys.time() -. t);
  fx 
;;
