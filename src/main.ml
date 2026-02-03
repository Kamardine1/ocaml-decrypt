(*
Auteur :
Mattéo Raffin
Kamardine Mirghane Mohamed
*)


#use "src/tools.ml";;
#use "src/Utils.ml";;



(* =================================== PREMIERE PARTIE =================================== *)


(* fonction auxilliaire de la fonction insert_data_in_var *)
let rec insert_data_in_var_aux(p_file , p_var : t_data list * t_data list ref) : t_data list =
  if p_file = []
    then !p_var
  else 
    if is_data_in_list(!p_var,(List.hd(p_file)))
    then insert_data_in_var_aux(List.tl(p_file),p_var)
    else
    (
    p_var := List.hd(p_file) :: insert_data_in_var_aux(List.tl(p_file),p_var);
      !p_var
  )
;;


(* fonction qui met les elements d'un fichier sur une variable *)
let insert_datas_in_var(p_file,p_var : string * t_data list ref) : t_data list =
  let l_tmp : 'a list ref = ref (read_data_from_file("Data/"^p_file))
  and l_data_file : t_data list ref = ref [] in
    for i=0 to len(!l_tmp)-1 do
      l_data_file := {login = Stdlib.fst(List.hd(!l_tmp)); pwd = Stdlib.snd(List.hd(!l_tmp))} :: !l_data_file;
      l_tmp := List.tl(!l_tmp)
    done;
    insert_data_in_var_aux(!l_data_file,p_var)
;;

(* fonction auxiliaire de la fonction fusion_files *)
let rec fusion_files_aux(p_files, res_final : string list * t_data list ref) : t_data list =
  if p_files = []
    then !res_final
  else 
    (    
      res_final := insert_datas_in_var(List.hd(p_files),res_final);
      fusion_files_aux(List.tl(p_files),res_final);
    )
;;

(* fonction qui fusionne les fichiers MAIS A L'ENVERS *)
let fusion_files(p_files : string list) : t_data list =
  let res_final : t_data list ref = ref [] in
  if p_files = []
    then invliste(!res_final)
  else fusion_files_aux(p_files,res_final)
;;

let fusion_files_of_app(app_name : string) : t_data list = fusion_files(transform_tab_to_list(insert_n_file_in_tab(app_name,number_of_file(app_name))));;

(* Permet de créer une liste contenant toutes les fuites des applications *)
let rec merge_app(app_list,l_res : string list * 'a list ref) : (string * (t_data list)) list =
  if app_list = []
    then invliste(!l_res)
  else (l_res := (List.hd(app_list),fusion_files_of_app(List.hd(app_list))) :: !l_res;
        merge_app(List.tl(app_list),l_res))
;;



(* =================================== DEUXIEME PARTIE =================================== *)


(* fonction qui permet d'obtenir, grace au login donnée en paramètre, la liste qui
contient le nom d'application et les mot de passe qui ont sont associé a ce login.
Pour faire cela elle vérifie si le login est bien dans la variable merge_app_file qui
est constitué des login et mot de passe de cette application, si c'est le cas alors
elle recupère les positions des endroits où elle a trouvé ces logins pour ensuite ajouté
leurs mot de passe au résultat, ainsi que le nom de l'application dans lequel elle 
regarde afin de savoir d'ou vient ce mot de passe*)
let check_login_across_breachs_aux(p_login,p_res,name_app,merge_app_file : string * ((string * string) list ref) * string * t_data list) : ((string * string) list) =
  if is_login_in_lists(merge_app_file,p_login)
    then (
      let pos_list : int list ref = ref (get_pos_log_list(p_login,merge_app_file,ref([]))) in
      for i = 0 to len(!pos_list)-1 do
        p_res := (name_app,pwd_of(p_login,[nth(merge_app_file,List.hd(!pos_list))])) :: !p_res;
        pos_list := List.tl(!pos_list)
      done
    );
  !p_res;;

(* fonction qui permet d'obtenir la liste des application et mot de passe qui ont pour 
login celui donné en paramètre 
elle utilise une fonction auxiliaire qui regarde pour une application donné ces informations *)
let check_login_across_breachs(p_login,app_merge : string * (string*(t_data list))list) : (string * (string * string) list) list =
  let l_res : 'a list ref = ref [] in
    for i = 0 to len(app_merge)-1 do
      l_res := concat(check_login_across_breachs_aux(p_login,ref([]),Stdlib.fst(nth(app_merge,i)),Stdlib.snd(nth(app_merge,i))),!l_res)
    done;
    [(p_login,!l_res)]
;;

(* Fonction qui fait exactement la même chose que check_login_across_breachs, a la
différence que cette fois ci, celle ci ne prends en paramètre qu'un login, cela permet
nottament lors de test de pouvoir essayer sans avoir a décrire tout les paramètres,
elle ne sera d'ailleurs pas beaucoup utiliser car nous aurons besoin de l'autre fonction
dans la suite *)
let launch_clab(p_login : string) : (string * (string * string) list) list =
  check_login_across_breachs(p_login,merge_app(get_name_list(),ref([])));;



(* =================================== TROISIEME PARTIE =================================== *)


let check_pwd_across_breachs_aux(p_pwd,p_res,name_app,merge_app_file : string * ((string * string) list ref) * string * t_data list) : ((string * string) list) =
  if is_pwd_in_lists(merge_app_file,p_pwd)
    then (
      let pos_list : int list ref = ref (get_pos_pwd_list(p_pwd,merge_app_file,ref([]))) in
      for i = 0 to len(!pos_list)-1 do
        p_res := (name_app,login_of(p_pwd,[nth(merge_app_file,List.hd(!pos_list))])) :: !p_res;
        pos_list := List.tl(!pos_list)
      done
    );
  !p_res;;


let check_pwd_across_breachs(p_pwd,app_merge : string * (string*(t_data list))list) : (string * (string * string) list) list =
  let l_res : 'a list ref = ref [] in
    for i = 0 to List.length(app_merge)-1 do
      l_res := concat(check_pwd_across_breachs_aux(p_pwd,ref([]),Stdlib.fst(nth(app_merge,i)),Stdlib.snd(nth(app_merge,i))),!l_res)
    done;
    [(p_pwd,!l_res)]
;;


let launch_cpab(p_pwd : string) : (string * (string * string) list) list =
  check_pwd_across_breachs(p_pwd,merge_app(get_name_list(),ref([])));;



(* =================================== QUATRIEME PARTIE =================================== *)


let matching_pwd_with_app_log(l_clear_pwd_list : string list ref) : (string * (string * (string * string) list) list) list =
  let l_res : (string * (string * (string * string) list) list) list ref = ref []
  and merge_app_file : 'a list = merge_app(get_name_list(),ref([])) in
    for i=0 to  len(!l_clear_pwd_list)-1 do (
      if !l_clear_pwd_list = [] then ()
      else (
        let l_tmp = check_pwd_across_breachs(hash_password(List.hd(!l_clear_pwd_list)),merge_app_file) in
          if Stdlib.snd(List.hd(l_tmp)) = [] then (l_clear_pwd_list := List.tl(!l_clear_pwd_list);)
          else (
              l_res := (List.hd(!l_clear_pwd_list),l_tmp) :: !l_res;
              l_clear_pwd_list := List.tl(!l_clear_pwd_list);
            )
      )
    )
    done;
  invliste(!l_res);;


let launch_mpwal(file : string) : (string * (string * (string * string) list) list) list =
  let l_file : string list ref = init_list_clear_pwd(file) in
    matching_pwd_with_app_log(l_file);;



(* Récupère une liste de couple (login, application) pour lesquelle le mot de passe 
corresponds au haché d'une liste de mot de passe en clair *)
let get_couple_app_log(file : string) : (string * string) list =
  let l_file : string list ref = init_list_clear_pwd(file) in
  let l_tmp : 'a list ref = ref (matching_pwd_with_app_log(l_file))
  and l_res : (string * string) list ref = ref [] in (
    for i=0 to len(!l_tmp)-1 do (
      l_res := List.hd(Stdlib.snd(List.hd(Stdlib.snd(List.hd(!l_tmp))))) :: !l_res;
      l_tmp := List.tl(!l_tmp))
    done;
    !l_res)
;;



(* =================================== PARTIE FINALE =================================== *)




let extract_crack_tuple(l_uplet_clear_hash_app_log : (string * (string * (string * string) list) list) list) : (string*string*string) list =
  let l_tmp : (string * (string * (string * string) list) list) list ref = ref l_uplet_clear_hash_app_log in
    let tuple : (string * (string * (string * string) list) list) ref = ref (List.hd(!l_tmp))
    and l_app_log : (string * string) list ref = ref (Stdlib.snd(List.hd(Stdlib.snd(List.hd(!l_tmp)))))
    and l_res : (string * string * string) list ref = ref [] in (
      let len_l_app_log : int = len(!l_app_log) in (
        for j=0 to len(!l_tmp)-1 do (
          tuple := List.hd(!l_tmp);
          for i=0 to len_l_app_log do (
            if i < len_l_app_log
              then (
                l_res := ( Stdlib.fst(List.hd(!l_app_log)), (* Application*)
                          Stdlib.snd(List.hd(!l_app_log)),  (* Login *)
                          Stdlib.fst(!tuple) (* Mot de passe craqué *) 
                        ) :: !l_res;
                l_app_log := List.tl(!l_app_log)
              );
            );
          done;
          l_app_log := Stdlib.snd(List.hd(Stdlib.snd(!tuple)));
          l_tmp := List.tl(!l_tmp);
          );
        done;
      );
      invliste(!l_res)
      )
;;
