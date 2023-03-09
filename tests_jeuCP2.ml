(*
open CPinter;;
open JeuCP2;;

 *)

(* ---------------------------- *)
(* test de : convert            *)
(* ---------------------------- *)

(* *)
let test_convert_fonctional_1(status: t_test_status): unit =
  let test_step : t_test_step = test_start(status, "convert_fonctional_1")
  and p : t_point = {x = 2; y = 2}
  and p_sorti : t_point = {x = 100; y = 100} in
  let test_result : t_point t_test_result = test_exec(test_step, convert, (p, {x = 0; y = 0}, dilat)) in
  (
    if test_is_success(test_result)
    then assert_equals(test_step, "carré (2;2)", test_get(test_result), p_sorti)
    else test_error(test_step);
    test_end(test_step)
  )
;;
(* auteur : Louis *)

(* ---------------------------- *)
(* test de : draw_absolute_pt   *)
(* ---------------------------- *)

let test_draw_absolute_pt_functional_1(status: t_test_status) : unit =
  let test_step : t_test_step = test_start(status, "draw_absolute_pt_functional_1")
  and p : t_point = {x = 2; y = 2} in
  let test_result : t_point t_test_result = test_exec(test_step, convert, (p, {x = 0; y = 0}, dilat)) in
  (
    if test_is_success(test_result)
    then
      (
        open_graph(320, 610);
        fill_rect(50, 50, 10, 450);
        fill_rect(260, 50, 10, 450);
        fill_rect(50, 50, 220, 10);
        draw_absolute_pt(p, {x = 0; y = 0}, dilat, black);
        print_string("Voyez vous un carré noir vide en (2;2) (oui/non)");
        let reponse : string = read_line() in
        assert_equals(test_step, "carré (2;2)", reponse, "oui")
      )
    else test_error(test_step);
    test_end(test_step)
  )
;;
(*auteurs : mélie et louis*)

(* ---------------------------- *)
(* test de : fill_absolute_pt   *)
(* ---------------------------- *)

let test_fill_absolute_pt_functional_1(status: t_test_status) : unit =
  let test_step : t_test_step = test_start(status, "fill_absolute_pt_functional_1")
  and p : t_point = {x = 2; y = 2} in
  let test_result : t_point t_test_result = test_exec(test_step, convert, (p, {x = 0; y = 0}, dilat)) in
  (
    if test_is_success(test_result)
    then
      (
        open_graph(320, 610);
        fill_rect(50, 50, 10, 450);
        fill_rect(260, 50, 10, 450);
        fill_rect(50, 50, 220, 10);
        fill_absolute_pt(p, {x = 0; y = 0}, dilat, black);
        print_string("Voyez vous un carré noir plein en (2;2) (oui/non)");
        let reponse : string = read_line() in
        assert_equals(test_step, "carré (2;2)", reponse, "oui")
      )
    else test_error(test_step);
    test_end(test_step)
  )
;;
(*auteurs : mélie et louis*)
(* ---------------------------- *)
(* test de :drawfill_absolute_pt*)
(* ---------------------------- *)

let test_drawfill_absolute_pt_functional_1(status: t_test_status) : unit =
  let test_step : t_test_step = test_start(status, "fill_absolute_pt_functional_1")
  and p : t_point = {x = 2; y = 2} in
  let test_result : t_point t_test_result = test_exec(test_step, convert, (p, {x = 0; y = 0}, dilat)) in
  (
    if test_is_success(test_result)
    then
      (
        open_graph(320, 610);
        fill_rect(50, 50, 10, 450);
        fill_rect(260, 50, 10, 450);
        fill_rect(50, 50, 220, 10);
        drawfill_absolute_pt(p, {x = 0; y = 0}, dilat, blue);
        print_string("Voyez vous un carré bleu avec une bordure noir en (2;2) (oui/non)");
        let reponse : string = read_line() in
        assert_equals(test_step, "carré (2;2)", reponse, "oui")
      )
    else test_error(test_step);
    test_end(test_step)
  )
;;
(* ---------------------------- *)
(*     fonction de test         *)
(* ---------------------------- *)

let test_run() : unit =
  let alltests : t_test_status = create_test_status() in
  (
    test_convert_fonctional_1(alltests);
    
    test_draw_absolute_pt_functional_1(alltests);
    test_fill_absolute_pt_functional_1(alltests);

    (* print des resultats de test (DOIT RESTER A LA FIN !!!) *)
    print_test_report(alltests)
  )
;;
