
  (* Examples of calculating COLIMITS and LIMITS in the category FinSet *)


    (* Example of a PUSHOUT in FinSet *)

 val a_set = set([just "A",just "B",just "C",just "D"]);
 val b_set = set([just "C",just "D",just "E",just "F",just "G"]);
 val c_set = set([just "B",just "E",just "F",just "G"]);

 fun f_fn (just "A") = just "C"
   | f_fn (just "B") = just "C" 
   | f_fn (just "C") = just "D" 
   | f_fn (just "D") = just "E";
 fun g_fn (just "A") = just "B"
   | g_fn (just "B") = just "E"
   | g_fn (just "C") = just "B" 
   | g_fn (just "D") = just "F";

  let val cocomplete_cat(_,set_colimit) =  cocomplete_FinSet in
    set_colimit(podiagram(FinSet)( set_mor(a_set,f_fn,b_set),
                                   set_mor(a_set,g_fn,c_set))) end;


     (* Example of a PULLBACK in FinSet *)

 fun f_fn (just "A") = just "B"
   | f_fn (just "B") = just "B" 
   | f_fn (just "C") = just "E" 
   | f_fn (just "D") = just "F";
 fun g_fn (just "C") = just "E"
   | g_fn (just "D") = just "G"
   | g_fn (just "E") = just "B" 
   | g_fn (just "F") = just "B"
   | g_fn (just "G") = just "G" ;

  let val complete_cat(_,set_limit) =  complete_FinSet in
    set_limit(pbdiagram(FinSet)( set_mor(a_set,f_fn,c_set),
                                 set_mor(b_set,g_fn,c_set))) end;
  

