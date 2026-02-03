(*
Auteur :
Mattéo Raffin
Kamardine Mirghane Mohamed
*)

#use "src/main.ml";;


let launch() : (string*string*string) list =
  let init_l_app = launch_mpwal("french_passwords_top20000.txt") in 
    extract_crack_tuple(init_l_app);;

launch();;


let launch2(file_name : string) : (string*string*string) list =
  let init_l_app = launch_mpwal(file_name) in 
    extract_crack_tuple(init_l_app);;

launch2("french_passwords_top20000.txt");;