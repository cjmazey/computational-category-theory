               

(*                COMPUTATIONAL CATEGORY THEORY
                           Oct. 1987
               Programming Language: Standard ML    *)

                (* Implicit Equality version *)

(* This file contains categories, functors, limits, colimits, the colimit
   extension theorem and the limit version by duality *)

               (* Needs file "basic" compiled first *)



(* 1. Categories and Functors. *)

(* module category *)

   datatype ('o,'m)Cat  =   cat of ('m->'o)*('m->'o)*('o->'m)*('m*'m->'m);
   
   datatype ('o,'m)Eq_Cat =  eq_cat of ('o,'m)Cat * ('m * 'm -> bool);

   (*   source :   ('o,'m)Cat -> ('m->'o) *)
   (*   target :   ('o,'m)Cat -> ('m->'o) *)
   (*   identity : ('o,'m)Cat -> ('o->'m) *)
   (*   compose :  ('o,'m)Cat -> (('m*'m) -> 'm) *)

   fun source(cat(s,_,_,_))   = s;
   fun target(cat(_,t,_,_))   = t;
   fun identity(cat(_,_,i,_)) = i;
   fun compose(cat(_,_,_,c))  = c;

(* end *)

(* module category_of_sets *)
   (* uses category, basic *)


   datatype 'alpha Set_Mor  =   
	set_mor of 'alpha Set *('alpha->'alpha) * 'alpha Set;

   exception non_composable_pair;

local 

   fun set_source(set_mor(x,_,_)) = x;

   fun set_target(set_mor(_,_,y)) = y;

   fun set_identity x  = set_mor(x,identity_fn,x);

   fun set_compose(set_mor(x,f,y),set_mor(u,g,v)) = 
           if u == y then set_mor(x,f o g,v)
           else raise non_composable_pair;

in

   val FinSet = 
       cat(set_source,set_target,set_identity,set_compose)
end;

   fun nil_mor x  = set_mor(nil_set,nil_fn,x);
 
   (*   singleton_mor : 'alpha * 'alpha * 'alpha Set -> 'alpha Set_Mor *)
   fun singleton_mor(a,b,S) = 
          set_mor(singleton a,constant(b),S);

   (*   OF : 'alpha Set_Mor * 'alpha -> 'alpha *)
   infix 2 OF ;
   fun (set_mor(_,f,_)) OF a = f a;

(* end *)

(* module unit_category *)
   (* uses category, basic *)

   datatype UnitObj  =   one
   datatype UnitMor  =   id_one

   (*   unit_category : (UnitObj,UnitMor)Cat *)
   val unit_category = 
        let val f = constant(one)  
	    and g = constant(id_one)
            and h = constant(id_one)
        in cat(f,f,g,h)
        end;
(* end *)

(* module functor *)
   (* uses basic, category *)

   datatype ('o,'m,'o1,'m1)Functor  =   
	   ffunctor of ('o,'m)Cat *('o->'o1)*('m->'m1)*('o1,'m1)Cat
   
   type ('o,'m)Endofunctor = ('o,'m,'o,'m)Functor

   (*   domain : ('o,'m,'o1,'m1)Functor -> ('o,'m)Cat *)
   (*   range :  ('o,'m,'o1,'m1)Functor -> ('o1,'m1)Cat *)

   fun domain(ffunctor(A,_,_,_)) = A;
   fun range (ffunctor(_,_,_,B)) = B;

   (*   ofo : ('o,'m,'o1,'m1)Functor*'o -> 'o1 *)
   infix 5 ofo 
   fun (ffunctor(_,Fo,_,_)) ofo ob = Fo ob ;

   (*   ofm : ('o,'m,'o1,'m1)Functor*'m -> m1 *)
   infix 5 ofm 
   fun (ffunctor(_,_,Fm,_)) ofm m = Fm m;

   (*   Fun_comp : ('o,'m,'o1,'m1)Functor*('o1,'m1,'o2,'m2)Functor ->
		   ('o,'m,'o2,'m2)Functor *)

   infix Fun_comp;

   fun (ffunctor(A,fo,fm,_)) Fun_comp  (ffunctor(_,go,gm,D)) = 
	       ffunctor(A,fo o go,fm o gm,D);

   datatype ('o,'m,'o1,'m1)Isomorphism  =  
          isomorphism of ('o,'m,'o1,'m1)Functor * ('o1,'m1,'o,'m)Functor;

(* end *)

(* module functor_examples *)
   (* uses functor, category, basic, unit_category *)

   (*   I : ('o,'m)Cat -> ('o,'m)Endofunctor *)
   fun I A = ffunctor(A,identity_fn,identity_fn,A);

   (*   K : ('o,'m)Cat -> ('o -> (UnitObj,UnitMor,'o,'m)Functor) *)
   fun K (A as cat(_,_,id,_))(a) = 
	ffunctor( unit_category,
		 fn one => a ,
		 fn id_one => id(a) ,
		 A ) ;

   (*   O : ('o,'m)Cat -> ('o,'m,UnitObj,UnitMor)Functor *)
   fun O A =
	 ffunctor(A,constant(one),constant(id_one),unit_category);

(* end *)

(* module dual_category *)
   (* uses category, functor *)

   (*   dual_Cat : ('o,'m)Cat -> ('o,'m)Cat *)
   fun dual_Cat(cat(s,t,i,c)) = cat(t,s,i,fn (f,g) => c(g,f)  );

   (*   dual_Fun : ('o,'m,'o1,'m1)Functor -> ('o,'m,'o1,'m1)Functor *)
   fun dual_Fun(ffunctor(A,fo,fm,B)) = ffunctor(dual_Cat(A),fo,fm,dual_Cat(B));

(* end *)


(* 2. Some Colimits and Limits. *)

(* module special_colimits *)
   (* uses category *)

   type ('o,'m)InitialObj = 'o * ('o->'m);
   type ('o,'m)Coproduct = 'o*'o -> ('o*'m*'m) * ('o*'m*'m->'m);
   type ('o,'m)Coequaliser = 'm*'m -> ('o*'m) * ('o*'m->'m);
   type ('o,'m)Pushout = 'm*'m -> ('m*'m) * ('m*'m->'m);

   datatype ('o,'m)IO_CP_CE_Cat  =   
     	   io_cp_ce_cat of  ('o,'m)Cat *
			    ('o,'m)InitialObj *
			    ('o,'m)Coproduct  *
			    ('o,'m)Coequaliser ;

(* end *)

(* module special_limits *)
   (* uses category, dual_category, special_colimits *)

   type ('o,'m)Terminal_Obj = 'o * ('o->'m);
   type ('o,'m)Product = 'o*'o -> ('o*'m*'m) * ('o*'m*'m->'m);
   type ('o,'m)Equaliser = 'm*'m -> ('o*'m) * ('o*'m-> 'm);
   type ('o,'m)Pullback = ('m*'m) -> ( ('m*'m) * (('m*'m)->'m) );

   datatype ('o,'m)TO_PR_EQ_Cat  =   
	   to_pr_eq_cat of ('o,'m)Cat *
			   ('o,'m)Terminal_Obj *
			   ('o,'m)Product *
		           ('o,'m)Equaliser ;

   (*   dual_TPE_Cat : ('o,'m)TO_PR_EQ_Cat -> ('o,'m)IO_CP_CE_Cat *)
   fun dual_TPE_Cat(to_pr_eq_cat(C,t,p,e)) =
	    io_cp_ce_cat(dual_Cat C ,t,p,e);
(* end *)


(* 3. Diagrams, Cocones and Colimits *)

(* module graphs_diagrams_cocones *)
   (* uses basic, functor, category_of_sets *)
		
   datatype Label  =   word of string | number of int;
   type Node  = Label;
   type Edge  = Label;

   datatype Graph  =   graph of  (Node Set) *	
			         (Edge Set)  *
			         (Edge->Node)*	
		                 (Edge->Node);

   datatype ('o,'m)Diagram  =  diagram of Graph*(Node->'o)*(Edge->'m);
        
   datatype ('o,'m)CoCone  =  cocone of 'o*('o,'m)Diagram*(Node->'m);

   (*   co_apex : ('o,'m)CoCone -> 'o *)
   fun co_apex(cocone(a,_,_)) = a;

   (*   base : ('o,'m)CoCone -> ('o,'m)Diagram *)
   fun base(cocone(_,d,_)) = d;

   (*   sides : ('o,'m)CoCone -> (Node -> 'm) *)
   fun sides(cocone(_,_,s)) = s;

   datatype ('o,'m)CoCone_Mor  =  cocone_mor of ('o,'m)CoCone*'m*('o,'m)CoCone;

   (*   co_apex_morphism : ('o,'m)CoCone_Mor -> 'm *)
   fun co_apex_morphism(cocone_mor(_,f,_)) = f;

   (*   apply_Fun_Diag : ('o,'m,'o1,'m1)Functor * ('o,'m)Diagram
                                                        -> ('o1,'m1)Diagram  *)
   fun apply_Fun_Diag(ffunctor(_,Fo,Fm,_), diagram(g,ffo,ffm)) =
			diagram(g,ffo o Fo,ffm o Fm);

   (*   apply_Fun_CoCone : ('o,'m,'o1,'m1)Functor * ('o,'m)CoCone
                                                         -> ('o1,'m1)CoCone  *)
   fun apply_Fun_CoCone(F as ffunctor(_,fo,fm,_), c) =
	   let val new_base = apply_Fun_Diag(F,base(c)) 
           in cocone(fo(co_apex c),new_base,(sides c) o fm)
           end;

   (*   apply_Fun_CoCone_Mor : ('o,'m,'o1,'m1)Functor * ('o,'m)CoCone_Mor -> 
		   ('o1,'m1)CoCone_Mor *)
   fun apply_Fun_CoCone_Mor(F,cocone_mor(cS,f,cT)) = 
	   cocone_mor(apply_Fun_CoCone(F,cS),(F ofm f),apply_Fun_CoCone(F,cT));

   (*   dual_Graph : Graph -> Graph *)
   fun dual_Graph(graph(N,E,s,t)) = graph(N,E,t,s);
	
   (*   dual_Diag : ('o,'m)Diagram -> ('o,'m)Diagram *)
   fun dual_Diag(diagram(g,nm,em)) =
	diagram(dual_Graph(g),nm,em);

(* end *)

(* module cocomplete_category *)
   (* uses category, graphs_diagrams_cocones *)
	
   type ('o,'m)Colimiting_CoCone = 
	   ('o,'m)CoCone * (('o,'m)CoCone -> ('o,'m)CoCone_Mor);

   type ('o,'m)Colimit = ('o,'m)Diagram -> ('o,'m)Colimiting_CoCone;
 
   datatype ('o,'m)CoComplete_Cat = 
                      cocomplete_cat of ('o,'m)Cat*('o,'m)Colimit;

   (*   cat_from : ('o,'m)CoComplete_Cat -> ('o,'m)Cat  *)
   fun cat_from(cocomplete_cat(C,_)) = C;

   (*   colimit_cocone : ('o,'m)CoComplete_Cat -> 
			   (('o,'m)Diagram -> ('o,'m)CoCone) *)
   fun colimit_cocone(cocomplete_cat(_,colim)) d  =
	   let val (c,_) = colim d  in c end;

   (*   colimit_object : ('o,'m)CoComplete_Cat -> 
			   (('o,'m)Diagram -> 'o) *)
   fun colimit_object C  d  = co_apex(colimit_cocone C  d);
 
   (*   universal_part : ('o,'m)CoComplete_Cat -> 
	(('o,'m)Diagram -> (('o,'m)CoCone -> ('o,'m)CoCone_Mor)) *)
   fun universal_part(cocomplete_cat(C,colim)) d  =
	   let val (_,univ) = colim d  in univ end;

(* end *)


(* 4. Examples of diagrams *)

(* module graph_examples *)
   (* uses category_of_sets, basic, graphs_diagrams_cocones *)

   (*   nil_graph : Graph *)
   val nil_graph = graph(nil_set,nil_set,nil_fn,nil_fn);

   (*   node_graph : Node -> Graph *)
   fun node_graph n = graph(set [n],nil_set,nil_fn,nil_fn);

   (*   cpgraph : Node * Node -> Graph *)
   fun cpgraph(n,m) = 
	   graph(set [n,m],nil_set,nil_fn,nil_fn);

   (*   cegraph : Node * Node * Edge * Edge -> Graph *)
   fun cegraph(n,m,p,q) = 
	   graph(set [n,m],set [p,q],constant(n),constant(m));

   (*   pograph : Node * Node * Node * Edge * Edge -> Graph *)
   fun pograph(l,corner,r,e1,e2) =
      	graph(set [l,corner,r],
              set [e1,e2], 
              fn _ => corner ,
              fn x => if (x=e1) then l else r  );

   (*   pbgraph : Node * Node * Node * Edge * Edge -> Graph *)
   fun pbgraph(l,c,r,f,g) = dual_Graph(pograph(l,c,r,f,g));

(* end *)

(* module diagram_examples *)
   (* uses basic, category, graphs_diagrams_cocones, graph_examples *)

   (*   nil_diagram : ('o,'m)Diagram *)
   val nil_diagram = diagram(nil_graph,nil_fn,nil_fn);

   (*   node_diagram : ('o,'m)Cat -> ('o -> ('o,'m)Diagram) *)
   fun node_diagram C (a) =
	   diagram( node_graph(word("solo")),
		    constant(a),
		    constant(identity C (a)) );

   (*   cpdiagram : 'o*'o -> ('o,'m)Diagram *)
   fun cpdiagram(a,b) = 
	   diagram( cpgraph(word("left"),word("right")),
		    fn word(x) => if x="left" then a else b ,
		    nil_fn );

   (*   cediagram : ('o,'m)Cat -> ( ('m*'m) -> ('o,'m)Diagram ) *)
   fun cediagram C (f,g) =
	  diagram(cegraph(word("left"),word("right"),
			  word("fmor"),word("gmor")),
		  fn word(x) => if x ="left" then  source C (f) 
			         else target C (f) ,
		  fn word(x) => if x="fmor" then f else g );

   (*   podiagram : ('o,'m)Cat -> ( ('m*'m) -> ('o,'m)Diagram) *)
   fun podiagram(cat(ss,tt,_,_))(m1,m2) =
	   diagram(pograph(word("left"),word("corner"),word("right"),
			   word("le"),word("re")),
		   fn word(x) => if x = "corner" then ss(m1)
                                  else if x = "left" then tt(m1)
                                       else tt(m2) ,
		   fn word(x) => if x = "le" then m1 else m2  );

   (*   pbdiagram : ('o,'m)Cat -> (('m*m) -> ('o,'m)Diagram) *)
   fun pbdiagram C (m1,m2) = dual_Diag(podiagram(dual_Cat(C))(m1,m2));

(* end *)


(* 5. Cocontinuity. *)

(* module cocontinuous_functor *)
   (* uses functor, cocomplete_category, category, basic *)

   datatype ('o,'m,'o1,'m1)CoContinuous_Functor  =   
        cocontinuous_functor of  ('o,'m,'o1,'m1)Functor *
			         (('o,'m)Colimiting_CoCone -> 
				      ('o1,'m1)Colimiting_CoCone) ;

   (*   ccFun_comp : ('o,'m,'o1,'m1)CoContinuous_Functor *
	      ('o1,'m1,'o2,'m2)CoContinuous_Functor ->
		 ('o,'m,'o2,'m2)CoContinuous_Functor  *)
   infix ccFun_comp;
   
   fun (cocontinuous_functor(F,p)) ccFun_comp  (cocontinuous_functor(G,q)) =
	   cocontinuous_functor(F Fun_comp G,p o q);

(* end *)

(* module cocontinuous_functor_examples *)
   (* uses cocontinuous_functor, category, functor, basic, 
	graphs_diagrams_cocones, functor_examples, 
	cocomplete_category *)

   (*   cocontinuous_I : ('o,'m)Cat -> ('o,'m,'o,'m)CoContinuous_Functor *)
   fun cocontinuous_I  A =
	   cocontinuous_functor(I A,identity_fn);
				
   (*   iso_cocontinuity :
	('o,'m,'o1,'m1)Isomorphism -> ('o,'m,'o1,'m1)CoContinuous_Functor *)
   fun iso_cocontinuity(isomorphism(I1,I2)) =
	 let val preservation =
             fn (cA,univ) =>
		   let val new_univ = fn cB1 => 
                                        let val cA1 = apply_Fun_CoCone(I2,cB1)
                                        in apply_Fun_CoCone_Mor(I1,univ(cA1))
                                        end
                   in (apply_Fun_CoCone(I1,cA),new_univ) 
                   end
         in cocontinuous_functor(I1,preservation)
         end;
(* end *)	

(* 6. Cones, Limits, Completeness val Continuity. *)

(* module complete_category *)
   (* uses graphs_diagrams_cocones, category, special_limits,
	cocomplete_category, basic, unit_category, functor,
	functor_examples *)

   datatype ('o,'m)Cone  =   cone of 'o * ('o,'m)Diagram * (Node->'m);
   datatype ('o,'m)Cone_Mor  =  
	   cone_mor of ('o,'m)Cone * 'm * ('o,'m)Cone;

   (*   apex : ('o,'m)Cone -> 'o *)
   (*   apex_morphism : ('o,'m)Cone_Mor -> 'm *)
   fun apex(cone(a,_,_)) = a;
   fun apex_morphism(cone_mor(_,f,_)) = f;

   fun apply_Fun_Cone (F,cone(a,base,sides)) =
           cone(F ofo a,apply_Fun_Diag (F,base),fn n => F ofm (sides n));

   type ('o,'m)Limiting_Cone =
	   ('o,'m)Cone * (('o,'m)Cone -> ('o,'m)Cone_Mor);

   type ('o,'m)Limit = ('o,'m)Diagram -> ('o,'m)Limiting_Cone;

   datatype ('o,'m)Complete_Cat  =   complete_cat of ('o,'m)Cat * ('o,'m)Limit;

   datatype ('o,'m,'o1,'m1)Continuous_Functor  =   
        continuous_functor of ('o,'m,'o1,'m1)Functor *
			    (('o,'m)Limiting_Cone -> 
				 ('o1,'m1)Limiting_Cone) ;

   fun continuous_I  A =
	   continuous_functor(I A,identity_fn);

   (*   complete_unit_cat : (UnitObj,UnitMor)Complete_Cat *)
   val complete_unit_cat =
	   let val lim =
               fn d => let val resultcone = cone(one,d,constant(id_one))
		            val universal =
                                    fn c1 => cone_mor(resultcone,id_one,c1)
		        in (resultcone,universal) 
                        end
	   in complete_cat(unit_category,lim)
           end;
(* end *)


(* 7. Constructing Colimits. *)

(* module colimit *)
   (* uses basic, category, graphs_diagrams_cocones, diagram_examples,
	special_colimits, cocomplete_category *)

   (*   multicoproduct : ('o,'m)IO_CP_CE_Cat  ->
		 ( ('o,'m)Diagram -> ('o,'m)Colimiting_CoCone ) *)

   fun multicoproduct(cC as io_cp_ce_cat(C,init,cp,_)) 
	             (d as diagram(graph(N,E,s,t),fo,fm)) =
       if is_nil_set N then
          let val (i,i_univ) = init
              val i_cocone = cocone(i,nil_diagram,nil_fn) 
          in (i_cocone, fn c1 => cocone_mor(i_cocone,i_univ(co_apex c1),c1)  )
          end
	else
	  let val (n,N1) = singleton_split N
	      val (c,univc) = multicoproduct(cC)(diagram(graph(N1,E,s,t),fo,fm))
	      val ((b,f,g),univcp) = cp(fo(n),co_apex(c))
	      val result_cocone = 
		      cocone( b, d,
			      fn m => if (m=n) then f
			               else compose C (sides(c)(m),g) )
	      val universal =
	              fn c1 =>
		         let val u = co_apex_morphism(univc(c1))
		             val v = univcp(co_apex(c1),sides(c1)(n),u)
                         in cocone_mor(result_cocone,v,c1)
                         end
          in (result_cocone,universal) 
          end;


   (*   add_edge : ('o,'m)IO_CP_CE_Cat ->
    	   (('o,'m)Colimiting_CoCone * Edge -> ('o,'m)Colimiting_CoCone)  *)

   fun add_edge(io_cp_ce_cat(C, _, _, ce)) ((c,univ),e) =
	   let val diagram(g,fo,fm) = base(c) 
	       val graph(node_set,edge_set,s,t) = g 
	       val ((b,h),ce_univ) = 
		   ce( sides(c)(s(e)),
		       compose C (fm(e),sides(c)(t(e))) ) 
	       val result_graph = 
	            graph(node_set,set [e] U edge_set,s,t)  
	       val resultdiag = 
	 	    diagram(result_graph,fo,fm) 
	       val result_cocone = 
		     cocone( b,
			     resultdiag,
			     fn p => compose C (sides(c)(p),h) ) 
	       val universal =			
	              fn c1 =>
	                  let val u = co_apex_morphism(univ(c1)) 
		              val v = ce_univ(co_apex(c1),u) 
                          in cocone_mor(result_cocone,v,c1)   
                          end
	   in (result_cocone,universal)
           end;


   (*   finite_colimit : ('o,'m)IO_CP_CE_Cat -> ('o,'m)Colimit *)

   fun  finite_colimit  cC  (d as diagram(graph(N,E,s,t),fo,fm)) =
        if is_nil_set E then multicoproduct(cC) d  
	else let val (e,E1) = singleton_split(E) 
	         val d1 = diagram(graph(N,E1,s,t),fo,fm)
	     in add_edge(cC)((finite_colimit cC d1),e) 
             end;

   (*   cocomplete_cat : ('o,'m)IO_CP_CE_Cat -> ('o,'m)CoComplete_Cat *)
   fun mk_cocomplete_cat(cC as io_cp_ce_cat(C,_,_,_)) =
	cocomplete_cat(C,finite_colimit(cC));

(* end *)


(* 8. Duality and the Construction of Limits. *)

(* module dual_of_colimits *)
   (* uses category, graphs_diagrams_cocones, functor, dual_category, 
	complete_category, cocomplete_category, cocontinuous_functor,
	special_limits, colimit, cocontinuous_functor_examples *)

   (*   dual_Colim_CoCone : ('o,'m)Colimiting_CoCone -> ('o,'m)Limiting_Cone *)
   fun dual_Colim_CoCone(cocone(a,D,f),u) = 
	let val result_cone = cone(a,dual_Diag D,f) 
	    val universal =
	            fn (c1 as cone(a1,D1,f1)) =>
		             let val c2 = cocone(a1,dual_Diag D1,f1) 
		             in cone_mor(c1,co_apex_morphism(u c2),result_cone)
                             end
        in (result_cone,universal)
        end;

   (*   dual_Colim:  ('o,'m)Colimit -> ('o,'m)Limit *)
   fun dual_Colim(f) = fn D => dual_Colim_CoCone(f(dual_Diag D)) ;

   (*   dual_CoComp_Cat : ('o,'m)CoComplete_Cat -> ('o,'m)Complete_Cat *)
   fun dual_CoComp_Cat(cocomplete_cat(Y,colim)) = 
	         complete_cat(dual_Cat Y, dual_Colim colim);

   (*   mk_complete_cat : ('o,'m)TO_PR_EQ_Cat -> ('o,'m)Complete_Cat *)
   fun mk_complete_cat(cC) =
                dual_CoComp_Cat(mk_cocomplete_cat(dual_TPE_Cat cC));

   (*   dual_Lim_Cone : ('o,'m)Limiting_Cone -> ('o,'m)Colimiting_CoCone *)
   fun dual_Lim_Cone(cone(a,D,f),u) = 
	let val result_cocone = cocone(a,dual_Diag D,f) 
	    val universal =
	           fn (c1 as cocone(a1,D1,f1)) =>
		            let val c2 = cone(a1,dual_Diag D1,f1) 
		            in cocone_mor(c1,apex_morphism(u c2),result_cocone)
                            end
	in (result_cocone,universal)
        end;

   (*   dual_Lim : ('o,'m)Limit -> ('o,'m)Colimit *)
   fun dual_Lim(f) = fn D => let val limcone = f (dual_Diag D)
                          in dual_Lim_Cone(limcone)
                          end;

   (*   dual_Comp_Cat : ('o,'m)Complete_Cat -> ('o,'m)CoComplete_Cat *)
   fun dual_Comp_Cat(complete_cat(C,lim)) = 
                  cocomplete_cat(dual_Cat C,dual_Lim lim);

   (*   dual_CoCon_Fun : ('o,'m,'o1,'m1)CoContinuous_Functor ->
		  ('o,'m,'o1,'m1)Continuous_Functor *)
   fun dual_CoCon_Fun(cocontinuous_functor(F,p)) = 
	  continuous_functor( dual_Fun F, 
			      fn lc => dual_Colim_CoCone(p(dual_Lim_Cone lc)));

   (*   dual_Con_Fun : ('o,'m,'o1,'m1)Continuous_Functor ->
		  ('o,'m,'o1,'m1)CoContinuous_Functor *)
   fun dual_Con_Fun(continuous_functor(F,p)) = 
	  cocontinuous_functor( dual_Fun F, 
		       fn cc => dual_Lim_Cone(p(dual_Colim_CoCone cc)));
 
   (*   dual_via_Iso: ('o,'m)Cat *
                      ('o1,'m1,'o,'m)Isomorphism *
                      ('o1,'m1)CoComplete_Cat
		                ->   ('o,'m)Complete_Cat *)
   fun dual_via_Iso(A,Iso as isomorphism(I1,I2),cocomplete_cat(C,colim)) =
	let val cocontinuous_functor(_,preservation) = iso_cocontinuity(Iso)
	    val colim1 = fn D => preservation(colim(apply_Fun_Diag(I2,D)))
	in complete_cat(A,dual_Colim colim1)
        end;
(* end *)


(* 9. Constructors for disjoint union and the like. *)

(* module names *)
   (* uses basic, category_of_sets *)
   
   datatype  'alpha Tag  =        just of 'alpha | 
			          ttrue | 	
			          ffalse |
			          pink of ('alpha Tag) |
			          blue of ('alpha Tag) |
			          pair of ('alpha Tag * 'alpha Tag) |
			          subset of (('alpha Tag) Set) |
			          function of ('alpha Tag * 'alpha Tag) list; 

   (* Tagging the elements of sets so that we may construct:
      primitive elements, the terminal object and truthvalues, coproducts,
      products, equalisers, and Hom-functors (and presheaves) respectively *)

(* end *)


(* 10. Bicompleteness of the Category of Finite Sets. *)

(* module cocomplete_category_of_sets *)
   (* uses basic, names, category, category_of_sets, colimit,
	graphs_diagrams_cocones, dual_of_colimits,
	cocomplete_category, dual_of_colimits, special_colimits *)

local

   (*   set_initial :    (('alpha Tag)Set,('alpha Tag)Set_Mor)InitialObj  *)
   (*   set_coproduct :  (('alpha Tag)Set,('alpha Tag)Set_Mor)Coproduct   *)
   (*   set_coequaliser: (('alpha Tag)Set,('alpha Tag)Set_Mor)Coequaliser *)

   val set_initial = (nil_set,nil_mor);

   fun set_coproduct(s,t) =
       let val u = (pink mapset s) U (blue mapset t)
           val univ = fn (v,set_mor(_,f,_),set_mor(_,g,_)) =>
                             let val fg = fn pink x => f x |
                                             blue x => g x
                             in set_mor(u,fg,v)
                             end
       in ((u,set_mor(s,pink,u),set_mor(t,blue,u)),univ)
       end;

   fun set_coequaliser(set_mor(S,f,T),set_mor(R,g,V)) =
       let val cat(_,_,id,comp) = FinSet
       in
        if is_nil_set S then
          ((T, id(T)), fn (_,j) =>j)
	else 
             if cardinality S = 1 then
	     let val (y,_) = singleton_split S
             in if (f y) = (g y) then ( (T,id(T)), fn (_,j) => j)
	        else let val W = T diff (singleton (g y))
	             in ((W, set_mor(T,
                                     fn z => if z = (g y) then f y else z,
                                     W)),
	                 fn (a,set_mor(_,j,_)) => set_mor(W,j,a) )  
                     end
             end
	     else
	       let val (P,Q) = split(S)
	           val ((_,h),univ)    = 
		     set_coequaliser(set_mor(P,f,T),set_mor(P,g,T))
	           val ((w1,h1),univ1) = 
		     set_coequaliser
		           (comp(set_mor(Q,f,T),h),comp(set_mor(Q,g,T),h)) 
	       in ((w1, comp(h,h1)), fn (a,j) => univ1(a,univ(a,j))) 
               end
       end;
     
   (*   io_cp_ce_FinSet : 
	   (('alpha Tag)Set,('alpha Tag)Set_Mor)IO_CP_CE_Cat *)
   val io_cp_ce_FinSet = 
	   io_cp_ce_cat( FinSet,
			 set_initial,
			 set_coproduct,
			 set_coequaliser );

in

   (*   cocomplete_FinSet : 
	   (('alpha Tag)Set,('alpha Tag)Set_Mor)CoComplete_Cat *)
   val cocomplete_FinSet = mk_cocomplete_cat(io_cp_ce_FinSet)

end;

(* end *)



(* module complete_category_of_sets *)
   (* uses category_of_sets, special_limits, complete_category, 
	names, basic, category, dual_of_colimits *)

local

   (*   set_terminal : ((string Tag)Set,(string Tag)Set_Mor)Terminal_Obj *)
   (*   set_product  : ((string Tag)Set,(string Tag)Set_Mor)Product      *)
   (*   set_equaliser: ((string Tag)Set,(string Tag)Set_Mor)Equaliser    *)

   val set_terminal =  
          let val t = set [ttrue] 
          in (t, fn a => set_mor(a,constant(ttrue),t) )
          end;

   fun set_product(a,b) =
         let val a_cross_b = set (map pair (list_set (a X b)))
             val proj_a =set_mor(a_cross_b,fn pair(y,z) => y ,a) 
             val proj_b =set_mor(a_cross_b,fn pair(y,z) => z ,b)
             val univ   =
	         fn (p,m1,m2) =>
                     set_mor(p,fn y => pair(m1 OF y,m2 OF y) ,a_cross_b)
         in ((a_cross_b,proj_a,proj_b),univ)
         end;

   fun set_equaliser(f,g) =
	let val a = source FinSet f
	    val e = a filtered_by (fn y => ((f OF y)=(g OF y)))
        in (( e, set_mor(e,identity_fn,a) ),
	    ( fn (e1,h1) => set_mor(e1,fn y => h1 OF y ,e)))
        end;

   (*   to_pr_eq_FinSet :
		((string Tag)Set,(string Tag)Set_Mor)TO_PR_EQ_Cat  *)
   val to_pr_eq_FinSet = 
	        to_pr_eq_cat( FinSet,
			      set_terminal,
			      set_product,
			      set_equaliser )
in

   (*   complete_FinSet : 
	   ((string Tag)Set,(string Tag)Set_Mor)Complete_Cat *)
   val complete_FinSet = mk_complete_cat(to_pr_eq_FinSet) 
end;

(* end *)


(* 11. Examples of Limits and Colimits. *)

(* module colimit_examples *)
   (* uses category, graphs_diagrams_cocones, diagram_examples,
	special_colimits, basic, cocomplete_category *)

   (*   initial : ('o,'m)CoComplete_Cat -> ('o,'m)InitialObj *)
   fun initial(cocomplete_cat(C,colim)) =
	   let val (c,univ) = colim(nil_diagram)
           in ( co_apex(c),
	         fn a => let val c1 = cocone(a,nil_diagram,nil_fn)
                          in co_apex_morphism(univ c1) end  )
           end;

   (*   coequaliser : ('o,'m)CoComplete_Cat -> ('o,'m)Coequaliser *)
   fun coequaliser(cocomplete_cat(C,colim))(f,g) =
	   let val d as diagram(_,_,edge_map)= cediagram(C)(f,g) 
	       val (c,univ) = colim(d) 
           in ((co_apex(c),sides(c)(word("right")) ),
	       fn (p,m) =>
	          let val c1 = 
                      cocone(p,d,fn word(x) =>
                                    if x="right" then m
                                    else compose(C)(edge_map(word("fmor")),m))
                  in co_apex_morphism(univ c1) 
                  end)
           end;
       
   (*   coproduct : ('o,'m)CoComplete_Cat -> ('o,'m)Coproduct *)
   fun coproduct(cocomplete_cat(C,colim))(a,b) =
	   let val d = cpdiagram(a,b) 
	       val (c,univ) = colim(d)
           in ((co_apex(c),sides(c)(word("left")),sides(c)(word("right")) ),
                 fn (p,m1,m2) =>
                      let val c1 =
                         cocone(p,d,fn word(x) => if x="left" then m1 else m2)
                      in co_apex_morphism(univ(c1))  
                      end)
           end;

   (*   pushout : ('o,'m)CoComplete_Cat -> ('o,'m)Pushout *)
   fun pushout(cocomplete_cat(C,colim))(ml,mr) =
	   let val d as diagram(_,_,edge_map) = podiagram(C)(ml,mr)
	       val (c,univ) = colim(d)
           in ((sides(c)(word("left")),sides(c)(word("right")) ),
                fn (m_1,m_2) =>
	           let val c1 =
                         cocone(target(C)(m_1),d, 
                                fn word(x) =>
                                      if x="left" then m_1
                                      else if x="right" then m_2
                                      else compose(C)(edge_map(word("le")),m_1))
                  in co_apex_morphism(univ c1)  
                  end )
          end;

   (*   pushout_pair : ('o,'m)CoComplete_Cat -> ( (m*m) -> (m*m) ) *)
   fun pushout_pair(cC)(f,g) =
	let val ((p,q),_) = pushout(cC)(f,g) in (p,q) end;

(* end *)

(* module limit_examples *)
   (* uses special_limits, dual_of_colimits, cocomplete_category,
	colimit_examples, complete_category, colimit *)

   (*   terminal :  ('o,'m)Complete_Cat -> ('o,'m)Terminal_Obj *)
   (*   product :   ('o,'m)Complete_Cat -> ('o,'m)Product      *)
   (*   equaliser : ('o,'m)Complete_Cat -> ('o,'m)Equaliser    *)
   (*   pullback :  ('o,'m)Complete_Cat -> ('o,'m)Pullback     *)

   fun terminal  lC  = initial    (dual_Comp_Cat lC);
   fun product   lC  = coproduct  (dual_Comp_Cat lC);
   fun equaliser lC  = coequaliser(dual_Comp_Cat lC);
   fun pullback  lC  = pushout    (dual_Comp_Cat lC);

(* end *)

(* module category_with_coproducts *)
   (* uses special_colimits, category, cocomplete_category, 
	colimit_examples *)

   type ('o,'m)Coproduct_Cone = ('o*'m*'m) * ( ('o*'m*'m) -> 'm )

   datatype ('o,'m)Coproduct_Cat  =   
	   coproduct_cat of ('o,'m)Cat
                            *('o,'m)InitialObj
                            *('o,'m)Coproduct

   (*   cat_of_cpcat : ('o,'m)Coproduct_Cat -> ('o,'m)Cat  *)
   fun cat_of_cpcat(coproduct_cat(C,_,_)) = C;

   (*   coproduct_of_cpcat : ('o,'m)Coproduct_Cat -> ('o,'m)Coproduct  *)
   fun coproduct_of_cpcat(coproduct_cat(_,_,cp)) = cp;

   (*   cp_first, cp_second : ('o,'m)Coproduct_Cat -> ('o*'o -> m) *)
   fun cp_first(coproduct_cat(_,_,cp)) (a,b) = 
			let val ((_,f,_),_) = cp(a,b) in f end;
   fun cp_second(coproduct_cat(_,_,cp))(a,b) = 
			let val ((_,_,g),_) = cp(a,b) in g end;


   (*   o_cp_o_within :  'o * 'o * ('o,'m)Coproduct_Cat -> 'o *)
   (*   m_cp_m_within :  'm * 'm * ('o,'m)Coproduct_Cat -> 'm *)
   (*   o_cp_m_within :  'o * 'm * ('o,'m)Coproduct_Cat -> 'm *)
   (*   m_cp_o_within :  'm * 'o * ('o,'m)Coproduct_Cat -> 'm *)

   fun o_cp_o_within (coproduct_cat(_,_,cp)) (a,b) =
			let val ((c,_,_),_) = cp(a,b) in c end ;

   fun m_cp_m_within (cC as coproduct_cat(C,_,cp) ) (f,g) = 
		   let val a = target(C)(f) 
                       val b = target(C)(g)
		       val (_,u) = cp(source(C)(f),source(C)(g)) 
		   in u(o_cp_o_within cC (a,b),
		        compose(C) (f,cp_first(cC)(a,b)),
		        compose(C) (g,cp_second(cC)(a,b)) )
                   end;

   fun o_cp_m_within cC (a,g) =
             m_cp_m_within cC (identity(cat_of_cpcat(cC))(a),g) ;

   fun m_cp_o_within cC (f,b) = 
             m_cp_m_within cC (f,identity(cat_of_cpcat(cC))(b)) ;


   (*   |+| :  'm * 'm * ('o,'m)Coproduct_Cat -> 'm *)
   fun |+| cC (f,g) =
        let val (_,univ) =
              coproduct_of_cpcat cC (source(cat_of_cpcat cC)f,
                                     source(cat_of_cpcat cC)g)
        in univ(target(cat_of_cpcat cC)(f),f,g)
        end;

   (*   mk_coproduct_cat : ('o,'m)CoComplete_Cat -> ('o,'m)Coproduct_Cat *)
   fun mk_coproduct_cat cC = 
	coproduct_cat(cat_from cC,initial cC,coproduct cC);

   (*   initial_obj : ('o,'m)CoComplete_Cat -> 'o *)
   (*   initial_mor : ('o,'m)CoComplete_Cat -> ('o->'m) *)

   fun initial_obj cC = let val (obj,_) = initial cC in obj end;
   fun initial_mor cC a = let val (_,u) = initial(cC) in u a end;

(* end *)

(* module category_with_products *)
   (* uses complete_category, special_limits, limit_examples, 
	cocomplete_category, dual_of_colimits, category,
	category_with_coproducts *)

   (*   o_prod_o_within :  'o * 'o * ('o,'m)Complete_Cat -> 'o *)
   (*   m_prod_m_within :  'm * 'm * ('o,'m)Complete_Cat -> 'm *)
   (*   o_prod_m_within :  'o * 'm * ('o,'m)Complete_Cat -> 'm *)
   (*   m_prod_o_within :  'm * 'o * ('o,'m)Complete_Cat -> 'm *)

   fun o_prod_o_within lC (a,b) = 
                o_cp_o_within (mk_coproduct_cat(dual_Comp_Cat lC)) (a,b);

   fun m_prod_m_within lC (f,g) = 
                m_cp_m_within (mk_coproduct_cat(dual_Comp_Cat lC)) (f,g);

   fun o_prod_m_within lC (a,g) =
                o_cp_m_within (mk_coproduct_cat(dual_Comp_Cat lC)) (a,g);

   fun m_prod_o_within lC (f,b) =
                m_cp_o_within (mk_coproduct_cat(dual_Comp_Cat lC)) (f,b);


   (*   terminal_obj : ('o,'m)Complete_Cat -> 'o *)
   (*   terminal_mor : ('o,'m)Complete_Cat -> (o->m) *)
   fun terminal_obj lC = let val (obj,_) = terminal lC in obj end;

   fun terminal_mor lC a = let val (_,u) = terminal lC in u a end;

(* end *)
 
