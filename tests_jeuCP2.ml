(*
open CPinter;;
open JeuCP2;;
 *)

(* ---------------------------- *)
(* test de : convert            *)
(* ---------------------------- *)

(* *)
let test_convert_fonctional_1(status: t_test_status): unit =
  let test_step : t_test_step =
    test_start(status, "convert_fonctional_1")
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

let test_draw_absolute_pt_functional_1(status: t_test_status) : unit =
  let test_step : t_test_step =
    test_start(status, "draw_absolute_pt_functional_1")
  let test_result : t_point t_test_result = test_exec(test_step, convert, (p, {x = 0; y = 0}, dilat)) in
  (
    if test_is_success(test_result)
    then assert_equals(test_step, "carré (2;2)", test_get(test_result), p_sorti)
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

    (* print des resultats de test (DOIT RESTER A LA FIN !!!) *)
    print_test_report(alltests)
  )
;;
