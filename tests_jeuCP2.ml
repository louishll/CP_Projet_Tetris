(*
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
    then assert_equals(test_step, "carr� (2;2)", test_get(test_result), p_sorti)
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
  let test_result : unit t_test_result = test_exec(test_step, draw_absolute_pt, (p, {x = 0; y = 0}, dilat, black)) in
  (
    if test_is_success(test_result)
    then
      (
        open_graph(320, 610);
        fill_rect(50, 50, 10, 450);
        fill_rect(260, 50, 10, 450);
        fill_rect(50, 50, 220, 10);
        draw_absolute_pt(p, {x = 0; y = 0}, dilat, black);
        print_string("Voyez vous un carr� noir vide en (2;2) (oui/non)");
        let reponse : string = read_line() in
        assert_equals(test_step, "carr� (2;2)", reponse, "oui")
      )
    else test_error(test_step);
    test_end(test_step);
    clear_graph();
    set_color(black);
  )
;;

(*auteurs : m�lie et louis*)

(* ---------------------------- *)
(* test de : fill_absolute_pt   *)
(* ---------------------------- *)

let test_fill_absolute_pt_functional_1(status: t_test_status) : unit =
  let test_step : t_test_step = test_start(status, "fill_absolute_pt_functional_1")
  and p : t_point = {x = 2; y = 2} in
  let test_result : unit t_test_result = test_exec(test_step, fill_absolute_pt, (p, {x = 0; y = 0}, dilat, black )) in
  (
    if test_is_success(test_result)
    then
      (
        open_graph(320, 610);
        fill_rect(50, 50, 10, 450);
        fill_rect(260, 50, 10, 450);
        fill_rect(50, 50, 220, 10);
        fill_absolute_pt(p, {x = 0; y = 0}, dilat, black);
        print_string("Voyez vous un carr� noir plein en (2;2) (oui/non)");
        let reponse_2 : string = read_line() in
        assert_equals(test_step, "carr� (2;2)", reponse_2, "oui")
      )
    else test_error(test_step);
    test_end(test_step);
    clear_graph();
    set_color(black);
  )
;;
(*auteurs : m�lie et louis*)
(* ---------------------------- *)
(* test de :drawfill_absolute_pt*)
(* ---------------------------- *)

let test_drawfill_absolute_pt_functional_1(status: t_test_status) : unit =
  let test_step : t_test_step = test_start(status, "drawfill_absolute_pt_functional_1")
  and p : t_point = {x = 2; y = 2} in
  let test_result : unit t_test_result = test_exec(test_step, drawfill_absolute, (p, {x = 0; y = 0}, dilat,blue)) in
  (
    if test_is_success(test_result)
    then
      (
        open_graph(320, 610);
        fill_rect(50, 50, 10, 450);
        fill_rect(260, 50, 10, 450);
        fill_rect(50, 50, 220, 10);
        drawfill_absolute_pt(p, {x = 0; y = 0}, dilat, blue);
        print_string("Voyez vous un carr� bleu avec une bordure noir en (2;2) (oui/non)");
        let reponse_3 : string = read_line() in
        assert_equals(test_step, "carr� (2;2)", reponse_3, "oui")
      )
    else test_error(test_step);
    test_end(test_step);
    clear_graph();
    set_color(black);
  )
;;
(*auteur : NICOLAS et M�LIE*)
(* ---------------------------- *)
(*     test draw relative       *)
(* ---------------------------- *)

let test_draw_relative_pt_functional_1(status: t_test_status) : unit =
  let test_step : t_test_step = test_start(status, "draw_relative_pt_functional_1")
  and p : t_point = {x = 1; y = 2} in
  let test_result : unit t_test_result = test_exec(test_step, draw_relative_pt, (p,{x = 0; y = 0}, {x = 0; y = 0}, dilat, blue)) in
  (
    if test_is_success(test_result)
    then
      (
        open_graph(320, 610);
        fill_rect(50, 50, 10, 450);
        fill_rect(260, 50, 10, 450);
        fill_rect(50, 50, 220, 10);
        draw_relative_pt(p,{x = 0; y = 0}, {x = 0; y = 0}, dilat, blue);
        print_string("Voyez vous un carr� bleu vide en (1;2) (oui/non)");
        let reponse_4 : string = read_line() in
        assert_equals(test_step, "carr� (1;2)", reponse_4, "oui")
      )
    else test_error(test_step);
    test_end(test_step);
    clear_graph();
    set_color(black);
  )
;;
(*auteur : NICOLAS et M�LIE*)
(* ---------------------------*)
(* test de : fill_relative_pt *)
(* ---------------------------*)

let test_fill_relative_pt_functional_1(status: t_test_status) : unit =
  let test_step : t_test_step = test_start(status, "fill_relative_pt_functional_1")
  and p : t_point = {x = 2; y = 2} in
  let test_result : unit t_test_result = test_exec(test_step, convert, (p,{x = 2; y = 2}, {x = 0; y = 0}, dilat, black)) in
  (
    if test_is_success(test_result)
    then
      (
        open_graph(320, 610);
        fill_rect(50, 50, 10, 450);
        fill_rect(260, 50, 10, 450);
        fill_rect(50, 50, 220, 10);
        fill_relative_pt(p,{x = 2; y = 2}, {x = 0; y = 0}, dilat, black);
        print_string("Voyez vous un carr� noir plein en (4;4) (oui/non)");
        let reponse_5 : string = read_line() in
        assert_equals(test_step, "carr� noir plein au dessus du carr� bleu", reponse_5, "oui")
      )
    else test_error(test_step);
    test_end(test_step);
     clear_graph();
    set_color(black);
  )
;;
(*auteur : m�lie*)
(* ---------------------------- *)
(*     test drawfill relative   *)
(* ---------------------------- *)

let test_drawfill_relative_pt_functional_1(status : t_test_status) : unit =
  let test_step : t_test_step = test_start(status, "drawfill_relative_pt_functional_1")
  and p : t_point = {x = 1; y = 1} in
  let test_result : t_point t_test_result = test_exec(test_step, convert, (p, {x = 0; y = 0}, dilat)) in
  (
    if test_is_success(test_result)
    then
      (
        open_graph(320, 610);
        fill_rect(50, 50, 10, 450);
        fill_rect(260, 50, 10, 450);
        fill_rect(50, 50, 220, 10);
        drawfill_relative_pt(p,{x = 1; y = 1}, {x = 0; y = 0}, dilat, blue);
        print_string("Voyez vous un carr� bleu avec une bordure noir en (2;2) (oui/non)");
        let reponse_6 : string = read_line() in
        assert_equals(test_step, "carr� (1;2)", reponse_6, "oui")
      )
    else test_error(test_step);
    test_end(test_step);
    clear_graph();
    set_color(black);
  )
;;

(*auteur : NICOLAS*)
(* ---------------------------- *)
(*     test draw_pt_list        *)
(* ---------------------------- *)

let test_draw_pt_list_functional_1(status : t_test_status) : unit =
  let test_step : t_test_step = test_start(status, "draw_pt_list_functional_1")
  and pt_list : t_point list = [{x = 0; y = 0};{x = 0; y = 1};{x = 0; y = 2}] in
  let test_result : unit t_test_result = test_exec(test_step, draw_pt_list, (pt_list,{x = 1; y = 1}, {x = 0; y = 0}, dilat, black)) in
  (
    if test_is_success(test_result)
    then
      (
        open_graph(320, 610);
        fill_rect(50, 50, 10, 450);
        fill_rect(260, 50, 10, 450);
        fill_rect(50, 50, 220, 10);
        draw_pt_list(pt_list,{x = 1; y = 1}, {x = 0; y = 0}, dilat, blue);
        print_string("Voyez vous trois carr� noir vide en ligne en (1;1) (oui/non)");
        let reponse_7 : string = read_line() in
        assert_equals(test_step, "carr� (1;2)", reponse_7, "oui")
      )
    else test_error(test_step);
    test_end(test_step);
    clear_graph();
    set_color(black);
  )
;;
(*auteur : NICOLAS*)
(* -----------------------*)
(* test de : fill_pt_list *)
(* -----------------------*)

let test_fill_pt_list_functional_1(status: t_test_status) : unit =
  let test_step : t_test_step = test_start(status, "fill_pt_list_functional_1")
  and pt_list : t_point list = [{x = 2; y = 2}; {x = 1; y = 1}; {x = 1; y = 2}] in
  let test_result : unit t_test_result = test_exec(test_step, fill_pt_list, (pt_list,{x = 2; y = 2}, {x = 0; y = 0}, dilat, red)) in
  (
    if test_is_success(test_result)
    then
      (
        open_graph(320, 610);
        fill_rect(50, 50, 10, 450);
        fill_rect(260, 50, 10, 450);
        fill_rect(50, 50, 220, 10);
        fill_pt_list(pt_list,{x = 2; y = 2}, {x = 0; y = 0}, dilat, red);
        print_string("Voyez vous 3 carr� noir plein) (oui/non)");
        let reponse_8 : string = read_line() in
        assert_equals(test_step, "carr� noir plein )", reponse_8, "oui")
      )
    else test_error(test_step);
    test_end(test_step);
    clear_graph();
    set_color(black);
  )
;;
(*auteur : m�lie*)

(* ---------------------------- *)
(*     fonction de test         *)
(* ---------------------------- *)

let test_run() : unit =
  let alltests : t_test_status = create_test_status() in
  (
    test_convert_fonctional_1(alltests);
    (*question1*)
    test_draw_absolute_pt_functional_1(alltests);
    test_fill_absolute_pt_functional_1(alltests);
    test_drawfill_absolute_pt_functional_1(alltests);
    (*question2*)
    test_draw_relative_pt_functional_1(alltests);
    test_fill_relative_pt_functional_1(alltests);
    test_drawfill_relative_pt_functional_1(alltests);
    (*question3*)
    test_draw_pt_list_functional_1(alltests);
    test_fill_pt_list_functional_1(alltests);
    test_drawfill_pt_list_functional_1(alltests);

    (* print des resultats de test (DOIT RESTER A LA FIN !!!) *)
    print_test_report(alltests)
  )
;;
