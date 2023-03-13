5;;(*pour ouvrir Ocaml*)
(*graphique t�tris
taille carr� : 20
marge droite : 50
marge gauche : 50
marge bas : 50
marge haut : 100
taille bord de zone : 10
grille du t�tris = 320 610
 *)

(* -------------------------- *)
(* -------------------------- *)
(*    fonctions utilitaires   *)
(* -------------------------- *)
(* -------------------------- *)

let mywait(x : float) : unit =
  let y : float ref = ref (Sys.time()) in
  while (Sys.time() -. !y) < x
  do ()
  done
;;

(* ------------------------------------------------- *)
(* ------------------------------------------------- *)
(*    Types, formes, parametrage et initialisation   *)
(* ------------------------------------------------- *)
(* ------------------------------------------------- *)


(* Types *)
type t_point = {x : int ; y : int} ;;

type 'a t_array = {len : int ; value : 'a array} ;;

type t_shape = {shape : t_point list ; x_len : int ; y_len : int ; 
                rot_rgt_base : t_point ; rot_rgt_shape : int ; 
                rot_lft_base : t_point ; rot_lft_shape : int} ;; 

type t_cur_shape = {base : t_point ref ; shape : int ref ; color : t_color ref} ;;


type t_param_time = {init : float ; extent : float ; ratio : float} ;;

type t_param_graphics = 
    {base : t_point ; dilat : int ; color_arr : t_color t_array} ;;

type t_param = 
  {time : t_param_time ; 
   mat_szx : int ; mat_szy : int ;
   graphics : t_param_graphics ; 
   shapes : t_shape t_array
} ;;

type t_play = {par : t_param ; cur_shape : t_cur_shape ; mat : t_color matrix} ;;


(* Initialisation de quelques formes et des parametres *)

let init_sh011() : t_shape = 
  {shape = [{x = 0 ; y = 0} ; {x = 1 ; y = 0} ; {x = 2 ; y = 0} ; {x = 3 ; y = 0}] ; 
  x_len = 4 ; y_len = 1 ; 
  rot_rgt_base = {x = 1 ;  y = 1} ; rot_rgt_shape = 1 ; 
  rot_lft_base = {x = 2 ; y = 1} ; rot_lft_shape = 1} 
;;
let init_sh112() : t_shape = 
  {shape = [{x = 0 ; y = 0} ; {x = 0 ; y = -1} ; {x = 0 ; y = -2} ; {x = 0 ; y = -3}] ; 
  x_len = 1 ; y_len = 4 ; 
  rot_rgt_base = {x = -2 ;  y = -1} ; rot_rgt_shape = 0 ; 
  rot_lft_base = {x = -1 ; y = -1} ; rot_lft_shape = 0} 
;;
let init_sh211() : t_shape = 
  {shape = [{x = 0 ; y = 0} ; {x = 0 ; y = -1} ; {x = 1 ; y = 0} ; {x = 1 ; y = -1}] ; 
  x_len = 2 ; y_len = 2 ; 
  rot_rgt_base = {x = 0 ;  y = 0} ; rot_rgt_shape = 2 ; 
  rot_lft_base = {x = 0 ;  y = 0} ; rot_lft_shape = 2} 
;;

let init_shapes() : t_shape t_array = 
  {len = 3 ; value = [| init_sh011() ; init_sh112() ; init_sh211() |]} 
;;
let init_color() : t_color t_array = 
  {len = 7 ; value = [|blue ; red ; green ; yellow ; cyan ; magenta ; grey|]} ;;

let init_param() : t_param = 
    {
    time = {init = 1.0 ; extent = 10.0 ; ratio = 0.8} ; 
    mat_szx = 15 ; mat_szy = 28 ;
    graphics = {base = {x = 50 ; y = 50} ; dilat = 20 ; color_arr = init_color()} ; 
    shapes = init_shapes()
    }
;;

(* --------------------------------- *)
(* --------------------------------- *)
(*   Types et fonctions graphique    *)
(* --------------------------------- *)
(* --------------------------------- *)


let dilat : int = 20 ;;
(*notre base_draw prend (0,0) comme valeur donc on ne l'utilise pas dans la fonction convert*)

let convert(p, base_draw, dilat : t_point * t_point * int) : t_point =
  {x = (p.x * dilat + 60); y = (p.y * dilat + 70)}
;;
(*AUTEUR : NICOLAS*)

(*QUESTION 1*)

let draw_absolute_pt(p, base_draw, dilat, col : t_point * t_point * int * t_color ) : unit =
  set_color(col);
  let new_p : t_point = convert(p, base_draw, dilat) in
  draw_rect(new_p.x, new_p.y, dilat - 1, dilat - 1)
;;
(*auteur : NICOLAS*)

let fill_absolute_pt(p, base_draw, dilat, col : t_point * t_point * int * t_color ) : unit =
  set_color(col);
  let new_p : t_point = convert(p, base_draw, dilat) in
  fill_rect(new_p.x, new_p.y, dilat - 1, dilat - 1)
;;
(*auteur : NICOLAS*)

let drawfill_absolute_pt(p, base_draw, dilat, col : t_point * t_point * int * t_color) : unit =
  fill_absolute_pt(p, base_draw, dilat, col);
  draw_absolute_pt(p, base_draw, dilat, 0)
;;
(*auteur : PIERRE*)

(*QUESTION 2*)

let draw_relative_pt(p, base_point, base_draw, dilat, col : t_point * t_point * t_point * int * t_color) : unit =
  let new_p : t_point = {x = p.x + base_point.x ; y = p.y + base_point.y} in
  draw_absolute_pt(new_p, base_draw, dilat, col)
;;
(*auteur : NICOLAS*)

let fill_relative_pt(p, base_point, base_draw, dilat, col : t_point * t_point * t_point * int * t_color) : unit =
  let new_p : t_point = {x = p.x + base_point.x ; y = p.y + base_point.y} in
  fill_absolute_pt(new_p, base_draw, dilat, col)
;;
(*auteur: PIERRE*)

let drawfill_relative_pt(p, base_point,  base_draw, dilat, col : t_point * t_point * t_point * int * t_color) : unit =
  fill_relative_pt(p, base_point, base_draw, dilat, col);
  draw_relative_pt(p, base_point, base_draw, dilat, 0)
;;
(*auteur : PIERRE*)


(*QUESTION 3*)

let draw_pt_list(pt_list, base_pt, base_draw, dilat, col : t_point list * t_point * t_point * int * t_color) : unit =
  (
    for i = 0 to len(pt_list) - 1 do
      draw_relative_pt(nth(pt_list, i), base_pt, base_draw, dilat, col)
    done;
  )
;;
(*auteur :NICOLAS*)

let fill_pt_list(pt_list, base_pt, base_draw, dilat, col : t_point list * t_point * t_point * int * t_color) : unit =
  (
    for i = 0 to len(pt_list) - 1 do
      fill_relative_pt(nth(pt_list, i), base_pt, base_draw, dilat, col)
    done;
  )
;;

(*auteur : NICOLAS-PIERRE*)

let drawfill_pt_list(pt_list, base_pt, base_draw, dilat, col : t_point list * t_point * t_point * int * t_color) : unit=
  fill_pt_list(pt_list,base_pt,base_draw,dilat,col);
  draw_pt_list(pt_list,base_pt,base_draw,dilat,0)
;;
(*auteur : PIERRE*)

(*Question 4*)

let draw_frame(base_draw, size_x, size_y, dilat: t_point * int * int * int) : unit =
  let list_left : t_point list ref = ref [{x = 0; y = 0}]
  and list_right : t_point list ref = ref [{x = size_x; y = 0}]
  and list_down : t_point list ref = ref [] in
  (
    for i = 1 to (size_y) do
      (
        list_left := add_lst(!list_left, {x = 0; y = i});
        list_right := add_lst(!list_right, {x = size_x; y = i})
      )
    done;
    for i = 1 to (size_x) do
      list_down := add_lst(!list_down, {x = i; y = 0})
    done;
    let base_pt : t_point = {x = 0; y = 0}
    and base_draw : t_point = {x = 0; y = 0} in
    (
      drawfill_pt_list(!list_left, base_pt, base_draw, dilat, black);
      drawfill_pt_list(!list_right, base_pt, base_draw, dilat, black);
      drawfill_pt_list(!list_down, base_pt, base_draw, dilat, black)
    )
  )
;;
(*auteur : Louis*)

(*Question 6*) (*auteur : Pierre*)

let getArrlen(prm : 'a t_array) : int =  prm.len;;
let getValue(prm : 'a t_array) : 'a array = prm.value;;
let getShape(prm : t_shape) : t_point list = prm.shape;;
let getShapeXlen(prm : t_shape) : int = prm.x_len;;
let getShapeYlen(prm : t_shape) : int = prm.y_len;;
let getCurBase(prm : t_cur_shape) : t_point ref = prm.base;;
let getCurShape(prm : t_cur_shape) : int ref = prm.shape;;
let getCurColor(prm : t_cur_shape) : t_color ref = prm.color;;
let getInitTime(prm : t_param_time) : float = prm.init;;
let getExtentTime(prm : t_param_time) : float = prm.extent;;
let getRatioTime(prm : t_param_time) : float = prm.ratio;;
let getGraphicBase(prm : t_param_graphics) : t_point = prm.base;;
let getGraphicDilat(prm : t_param_graphics) : int = prm.dilat;;
let getGraphicColor(prm : t_param_graphics) : t_color t_array = prm.color_arr;;
let getParamTime(prm : t_param) : t_param_time = prm.time;;
let getSizeX(prm : t_param) : int = prm.mat_szx;;
let getSiezY(prm : t_param) : int = prm.mat_szy;;
let getGraphics(prm : t_param) : t_param_graphics = prm.graphics;;
let getShapes(prm : t_param) : t_shape t_array = prm.shapes;;
let getParam(prm : t_play) : t_param = prm.par;;
let getCurShape(prm : t_play) : t_cur_shape = prm.cur_shape;;
let getMat(prm : t_play) : t_color matrix = prm.mat;;

(*Question 7*)


(*AUTEUR : Louis *)


(* ----------------------------------------------- *)
(* ----------------------------------------------- *)
(*    Deplacements et controle des deplacements    *)
(* ----------------------------------------------- *)
(* ----------------------------------------------- *)


(*
(* choix des deplacements suivant le caractere saisi *)
let move(pl, dir : t_play * char) : bool = 
  (
  if dir = 't'
    then rotate_right(pl)
    else
      if dir = 'c'
      then rotate_left(pl)
      else
        if dir = 'd'
        then move_left(pl)
        else
          if dir = 'h'
          then move_right(pl)
          else () ;  
  (dir = 'v')
  )
;;


(* ----------------------------------- *)
(* ----------------------------------- *)
(*    Suppression des lignes pleines   *)
(* ----------------------------------- *)
(* ----------------------------------- *)


(* --------------------- *)
(* --------------------- *)
(*   Une etape de jeu    *)
(* --------------------- *)
(* --------------------- *)

let newstep(pl, new_t, t, dt : t_play * float ref * float * float) : bool = 
  let the_end : bool ref = ref (!new_t -. t > dt) and dec : bool ref = ref false in
  let dir : char ref = ref 'x' and notmove : bool ref = ref false in
    (
    while not(!the_end)
    do 
      if key_pressed()
      then dir := read_key()
      else () ;
      dec := move(pl, !dir) ;
      dir := 'x' ; 
      new_t := Sys.time() ;
      the_end := !dec || (!new_t -. t > dt) ;
    done ; 
    if !dec 
    then (move_at_bottom(pl) ; notmove := true)
    else notmove := not(move_down(pl)) ;
    if !notmove
    then the_end := final_newstep(pl)
    else the_end := false;
    !the_end ;
    )
;;

(* ------------------------ *)
(* ------------------------ *)
(*    Fonction principale   *)
(* ------------------------ *)
(* ------------------------ *)


let jeuCP2() : unit =
  let pl : t_play = init_play() in
  let t : float ref = ref (Sys.time()) and new_t : float ref = ref (Sys.time()) in
  let dt : float ref = ref (time_init(pl.par)) and t_acc : float ref = ref (Sys.time()) in
  let the_end : bool ref = ref false in
    while not(!the_end)
    do
      the_end := newstep(pl, new_t, !t, !dt) ; 
      if ((!new_t -. !t_acc) > time_extent(pl.par))
      then 
        (
        dt := !dt *. time_ratio(pl.par) ; 
        t_acc := !new_t
        ) 
      else () ;
      t := !new_t
    done
;;
 *)
