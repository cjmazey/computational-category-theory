
       (* Further examples of LIMITS and COLIMITS in FinSet *)


                 (* Tends to run fairly slowly ! *)

   (* An example DIAGRAM *)

    val G = let val N = set([word "a",word "b",word "c",word "d"]);
                val E = set([word "f",word "g",word "h", word "i"]);;
                fun src (word "f") = word "a"
                  | src (word "g") = word "b"
                  | src (word "h") = word "a"
                  | src (word "i") = word "b";
                fun tgt (word "f") = word "c" 
                  | tgt (word "g") = word "c"
                  | tgt (word "h") = word "d"
                  | tgt (word "i") = word "d"
            in graph(N,E,src,tgt) end;

    val d_set = set([just "A",just "B",just "C",just "D"]);

    fun f_fn (just "A") = just "B"
      | f_fn (just "B") = just "B" 
      | f_fn (just "C") = just "E" 
      | f_fn (just "D") = just "F";
    fun g_fn (just "C") = just "B"
      | g_fn (just "D") = just "F"
      | g_fn (just "E") = just "B" 
      | g_fn (just "F") = just "E"
      | g_fn (just "G") = just "F" ;
    fun h_fn (x) = x;
    fun i_fn (just "C") = just "A"
      | i_fn (just "D") = just "A" 
      | i_fn (just "E") = just "B"
      | i_fn (just "F") = just "C" 
      | i_fn (just "G") = just "D";

    fun nodes_to_sets (word "a") = a_set
      | nodes_to_sets (word "b") = b_set
      | nodes_to_sets (word "c") = c_set
      | nodes_to_sets (word "d") = d_set;
    fun edges_to_arrows (word "f") = 
              set_mor(a_set,f_fn,c_set) 
      | edges_to_arrows (word "g") = 
              set_mor(b_set,g_fn,c_set)
      | edges_to_arrows (word "h") = 
              set_mor(a_set,h_fn,d_set)
      | edges_to_arrows (word "i") = 
              set_mor(b_set,i_fn,d_set);

    val D = diagram(G,nodes_to_sets,edges_to_arrows);

       (* COLIMIT of D *)

    let val cocomplete_cat(_,set_colimit) =  cocomplete_FinSet in
         set_colimit(D) end;

       (* LIMIT of D *)

    let val complete_cat(_,set_limit)     =  complete_FinSet in
         set_limit(D) end;

