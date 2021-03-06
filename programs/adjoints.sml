
                      (* Adjoint Functors *)

(* Contains types corresponding to various descriptions of ADJUNCTIONS
and coding of equivalences. Also categories of adjunctions and monads,
categories of algebras of monads and Kleisli categories *)

          (* Needs "basic", "cat", "comma" and "functorcat"*)

(* module adjunction *)
   (* uses ffunctor, category_of_functors, comma_category,
	special_comma_categories *)

   type ('o,'m,'o1,'m1)Universal_Morphism =
                      'o -> ( ('o1*'m) * ( ('o1*'m) -> 'm1 ) );

   datatype ('o,'m,'o1,'m1)Adjunction  =   
	adjunction of ('o,'m,'o1,'m1)Functor * 		
		      ('o1,'m1,'o,'m)Functor * 		
		      ('o,'m,'o,'m)Nat_transform * 	
		      ('o1,'m1,'o1,'m1)Nat_transform;


   datatype ('o,'m,'o1,'m1)Adjunction2  =  
	adjunction2 of ('o1,'m1,'o,'m)Functor *
                       ('o,'m,'o1,'m1)Universal_Morphism;

   datatype ('o,'m,'o1,'m1)Adjunction3  =  
	adjunction3 of ('o,'m,'o1,'m1)Functor *
		       ('o1,'m1,'o,'m)Functor *
		       ( ('o*'m*'o1),  ('o,'m,'o1,'m1)Right_Comma_Mor,
			 ('o*'m1*'o1), ('o,'m,'o1,'m1)Left_Comma_Mor )Functor *
		       ( ('o*'m1*'o1), ('o,'m,'o1,'m1)Left_Comma_Mor,
    			 ('o*'m*'o1),  ('o,'m,'o1,'m1)Right_Comma_Mor )Functor;
(* end *)


(* module UM_to_ADJ *)
   (* uses category, category_of_functors, category_of_adjunctions, 
	functor, functor_examples, adjunction *)

local 

   (*   free_functor : ('o,'m,'o1,'m1)Adjunction2 -> ('o,'m,'o1,'m1)Functor *)

   fun free_functor(adjunction2(G,universal)) =
	     let val A = domain(G) 
	         val B = range(G) 
	         val obj_part = fn b =>let val ((a,_),_) = universal(b)
                                        in a end
	         val mor_part =
		  fn m =>
		     let val (_,source_univ) = universal(source(B)(m)) 
		         val ((a,eta),_)     = universal(target(B)(m)) 
                     in source_univ(a,compose(B)(m,eta)) 
                     end
            in ffunctor(B,obj_part,mor_part,A)
            end;

in

   (*   UM_to_ADJ : ('o,'m,'o1,'m1)Adjunction2 -> ('o,'m,'o1,'m1)Adjunction *)
   fun UM_to_ADJ(adj2 as adjunction2(G,universal)) =
           let val A = domain(G)
               val B = range(G)
	       val F = free_functor(adj2) 
	       val eta = nat_transform
			( I(B),
			  fn b => let val ((_,f),_) = universal(b) in f end ,
			   F Fun_comp G )
	       val epsilon = 
		      nat_transform
			( G Fun_comp F,
			  fn a =>
			     let val (_,univ) = universal(G ofo a)
                             in univ(a,identity(B)(G ofo a))
                             end ,
			  I(A) ) 
         in adjunction(F,G,eta,epsilon)
         end
end;

(* end *)

(* module ADJ_to_ISO *)
   (* uses adjunction, ffunctor, comma_category, category,
	 special_comma_categories, category_of_functors *)

   (*   sharp : ('o,'m,'o1,'m1)Adjunction ->
		( ('o*'m*'o1),  ('o,'m,'o1,'m1)Right_Comma_Mor,
                  ('o*'m1*'o1), ('o,'m,'o1,'m1)Left_Comma_Mor )Functor *)

   fun sharp (adjunction(F,G,eta,epsilon)) =
	let val object_function =
	      fn (z,g,a) => 
		(z, compose(range F)( (F ofm g), (epsilon component_at a)), a) 
	    val morphism_function =
	      fn comma_mor(S,(s,f),T) =>
		comma_mor(object_function(S),
			  (s,f),
			  object_function(T) )   
       in ffunctor( right_comma_cat(G),
		   object_function,
                   morphism_function, 
		   left_comma_cat(F) )	
       end;


   (*   inv_sharp : ('o,'m,'o1,'m1)Adjunction ->
		( ('o*'m1*'o1), ('o,'m,'o1,'m1)Left_Comma_Mor,
                  ('o*'m*'o1),  ('o,'m,'o1,'m1)Right_Comma_Mor )Functor *)

   fun inv_sharp(adjunction(F,G,eta,epsilon)) =
	let val object_function =
	     fn (z,g,a) => 
		(z,compose(range G)((eta component_at z),(G ofm g)),a)
	    val morphism_function =
	     fn comma_mor(S,(s,f),T) =>
		comma_mor( object_function(S),
			   (s,f),
			   object_function(T) )   
      in ffunctor( left_comma_cat(F),
		  object_function,morphism_function,
		  right_comma_cat(G) )
      end;

   (*   ADJ_to_ISO : ('o,'m,'o1,'m1)Adjunction -> ('o,'m,'o1,'m1)Adjunction3 *)

   fun ADJ_to_ISO(adj as adjunction(F,G,_,_)) = 
	   adjunction3(F,G,sharp(adj),inv_sharp(adj));

(* end *)

(* module ISO_to_UM *)
   (* uses adjunction, functor, category *)

local

   (*   mk_universal_mor : 
	('o,'m,'o1,'m1)Adjunction3 -> ('o,'m,'o1,'m1)Universal_Morphism *)
   fun mk_universal_mor(adjunction3(F,G,sh,inv_sh))(z)  =
	let val C = range(F)
        in
	   (((F ofo z), 
             let val (_,f,_) = inv_sh ofo (z,identity(C)(F ofo z), F ofo z) 
             in f
             end),
	   ( fn (b,f) => let val (_,g,_) = sh ofo (z,f,b) in g end ))
        end
in

   (*   ISO_to_UM : ('o,'m,'o1,'m1)Adjunction3 -> ('o,'m,'o1,'m1)Adjunction2 *)
   fun ISO_to_UM(adj3 as adjunction3(_,G,_,_)) =
	   adjunction2(G,mk_universal_mor(adj3))
end;

(* end *)


(*  Constructions of Categories: Adjunction Categories. *)

(* module category_of_adjunctions *)
   (* uses category, functor, category_of_functors, adjunction, dual_category *)

   datatype ('o,'m,'o1,'m1)Adj_Mor  =   
	   adj_mor of ('o,'m,'o1,'m1)Adjunction *
	 	      ( ('o,'m,'o1,'m1)Nat_transform *
		        ('o1,'m1,'o,'m)Nat_transform ) *
		      ('o,'m,'o1,'m1)Adjunction ;

   (*   cat_of_adjunctions : 
	   ('o,'m)Cat*('o1,'m1)Cat -> 
		(('o,'m,'o1,'m1)Adjunction,('o,'m,'o1,'m1)Adj_Mor)Cat *)
   fun cat_of_adjunctions(A,B) =
	   let val cat(_,_,iAB,compAB) = cat_of_functors(A,B)
	       val cat(_,_,iBA,compBA) = cat_of_functors(B,A)
           in
	cat(fn adj_mor(s,_,_) => s ,		
	    fn adj_mor(_,_,t) => t ,		
	    fn Y as adjunction(F,G,_,_) => 
		adj_mor(Y,(iAB(F),iBA(G)),Y) ,
	    fn (adj_mor(Y,(sigma,tau),_),adj_mor(_,(sigma1,tau1),Z)) =>
		adj_mor( Y,
			 (compAB (sigma,sigma1),compBA (tau1,tau)),
			 Z )   )
           end;

(* end *)


(*  Monads. *)

(* module monads *)
   (* uses category_of_adjunctions, category_of_functors, 
	functor, category, adjunction *)

   datatype ('o,'m)Monad  =   
	   monad of ('o,'m)Endofunctor *
		    ('o,'m,'o,'m)Nat_transform *
		    ('o,'m,'o,'m)Nat_transform ;

   datatype ('o,'m)Monad_Mor  =  
	   monad_mor of ('o,'m)Monad *
		        ('o,'m,'o,'m)Nat_transform *
		        ('o,'m)Monad ;

   (*   cat_from_monad : ('o,'m)Monad -> ('o,'m)Cat *)
   fun cat_from_Monad(monad(T,_,_)) = domain(T);

   (*   cat_of_monads : ('o,'m)Cat -> 
	   ( ('o,'m)Monad, ('o,'m)Monad_Mor )Cat *)
   fun cat_of_monads(C) =
	      let val cat(_,_,id,comp) = cat_of_functors(C,C)
              in
	   cat( fn monad_mor(S,_,T) => S ,
		fn monad_mor(S,_,T) => T ,
		fn M as monad(T,_,_) =>
		    monad_mor(M,id(T),M) ,
		fn (monad_mor(S,f,_), monad_mor(_,g,T)) =>
		    monad_mor(S,comp(f,g),T) )
              end;
(* end *)


(*  Algebras. *)

(* module category_of_algebras *)
   (* uses category, functor, monads *)

   datatype ('o,'m)Algebra  =   algebra of 'o * 'm;
   datatype ('o,'m)Algebra_Mor  = 
       algebra_mor of (('o,'m)Algebra * 'm * ('o,'m)Algebra);

   (*   cat_of_algebras : ('o,'m)Monad -> 
                          (('o,'m)Algebra,('o,'m)Algebra_Mor)Cat *)
   fun cat_of_algebras(M) =
	      let val C = cat_from_Monad(M)
              in
	   cat( fn algebra_mor(A,_,_) => A ,
		fn algebra_mor(_,_,A) => A ,
	 	fn A as algebra(a,f) => 
		   algebra_mor(A,identity(C)(a),A) ,
		fn (algebra_mor(A,f,_),algebra_mor(_,g,B)) =>
		   algebra_mor(A,compose(C)(f,g),B)  )
              end;

   (*   carrier : ('o,'m)Algebra -> 'o *)
   fun carrier(algebra(a,_)) = a;

(* end *)


(*  Constructions of Categories: Kleisli Categories. *)

(* module Kleisli *)
   (* uses category, adjunction, monads, functor, category_of_functors *)

   datatype ('o,'m)Substitution  =   subst of 'o * 'm * 'o;

   (*   Kleisli : ('o,'m)Monad -> ('o,('o,'m)Substitution)Cat *)
   fun Kleisli(M as monad(T,eta,mu)) =
       	   let val C = domain(T)
           in
	cat( fn subst(a,f,b) => a ,
	     fn subst(a,f,b) => b ,
	     fn a => subst(a,(eta component_at a),a) ,
	     fn (subst(a,f,_),subst(_,g,b)) =>
		subst( a,
		       compose(C)(f,compose(C)(g,(mu component_at b))),
		       b )   )
           end;

(* end *)


(*  Monad derived from Adjunction. *)

(* module ADJ_to_MT *)
   (* uses adjunction, monads, category_of_functors, category, functor *)

   (*   ADJ_to_MT : ('o,'m,'o1,'m1)Adjunction -> ('o,'m)Monad *)
   fun ADJ_to_MT(adjunction(F,G,eta,epsilon)) =
           monad( F Fun_comp G, eta, F Fun_Nat_comp epsilon Nat_Fun_comp G );
(* end *)


