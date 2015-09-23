            (* Example of PUSHOUT of GRAPHS 
                 using colimits in COMMA CATEGORIES *)

   (* Some print functions *)

   fun tprint(x) = case x of
                 just(a) => "just " ^ a |
                 ttrue => "ttrue" |
                 blue(a) => "blue " ^ tprint(a) |
                 pink(a) => "pink " ^ tprint(a) | 
                 pair(a,b) => "pair(" ^ tprint(a) ^ "," ^tprint(b) ^ ")";

   val sprint = enumerate(tprint);


   (* Projections etc *)

   fun edges(E,_,_) = E;
   fun nodes(_,_,N) = N;

   fun mkpair(x,a,y,b) = 
     let val top = set [pair(x,y)];
         val xmor = set_mor(top, fn z => x,a);
         val ymor = set_mor(top, fn z => y,b);
         val (_,univ) = product(complete_FinSet)(a,b) in
     (univ(top,xmor,ymor) OF pair(x,y)) end;


   (* Two graph arrows with common source *)
   
   val a_graph = 
       let val Na = set [just "A",just "B",just "C"] ;
           val Ea = set [just "E",just "F"] in
      (Ea,
       set_mor(Ea,fn just "E" => mkpair(just "B",Na,just "A",Na) |
                     just "F" => mkpair(just "B",Na,just "C",Na), cross_product ofo Na),
       Na) end;

   val b_graph = 
       let val Nb = set [just "A",just "B",just "C",just "D"] ;
           val Eb = set [just "E",just "F",just "G",just "H"] in
      (Eb,
       set_mor(Eb,fn just "E" => mkpair(just "B",Nb,just "A",Nb) |
                     just "F" => mkpair(just "B",Nb,just "C",Nb) |
                     just "G" => mkpair(just "A",Nb,just "C",Nb) |
                     just "H" => mkpair(just "B",Nb,just "D",Nb),cross_product ofo Nb),
       Nb) end;

   val c_graph = 
       let val Nc = set [just "A",just "B",just "D"] ;
           val Ec = set [just "F",just "G",just "H"] in
      (Ec,
       set_mor(Ec,fn just "F" => mkpair(just "A",Nc,just "B",Nc) |
                     just "G" => mkpair(just "A",Nc,just "D",Nc) |
                     just "H" => mkpair(just "B",Nc,just "D",Nc),cross_product ofo Nc),
       Nc) end;

   val f_mor = let val fN_fn = fn x => x
                   val fE_fn = fn x => x in
         comma_mor( a_graph,
                    ( set_mor(edges(a_graph),fE_fn,edges(b_graph)),
                      set_mor(nodes(a_graph),fN_fn,nodes(b_graph)) ),
                    b_graph ) end;
   val g_mor = let fun gN_fn(just "A") = just("B")
                     | gN_fn(just "B") = just("A")
                     | gN_fn(just "C") = just("B")
                   fun gE_fn(just "E") = just("F")
                     | gE_fn(just "F") = just("F")   in
          comma_mor( a_graph,
                     ( set_mor(edges(a_graph),gE_fn,edges(c_graph)),
                       set_mor(nodes(a_graph),gN_fn,nodes(c_graph)) ),
                     c_graph ) end;

   (* Calculating the PUSHOUT *)

   let val cocomplete_cat(FinGraph,graph_colimit) = cocomplete_cat_of_graphs in
           graph_colimit(podiagram(FinGraph)(f_mor,g_mor)) end;
