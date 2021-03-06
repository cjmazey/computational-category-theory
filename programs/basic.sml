          (*  CATEGORICAL PROGRAMS 0 *)


(* This is the file which should be compiled as a preliminary to the
   categorical programs. It contains operations on lists and sets *)

fun length nil = 0 |
    length (h :: t) = 1 + length t;

fun member (e, nil) = false |
    member (e , (h :: t)) = if (e=h) then true
                                     else member (e, t) ;

fun flat1 [] = [] |
    flat1 (h :: t) = h @ (flat1 t);

fun split_list [] = ([],[])  |
    split_list (h :: t) =
          let val (a,b) = split_list t 
          in
              if (length t) mod 2 = 0 then ([h] @ a,b)
              else (a,[h] @ b)
          end;

fun singlify  [] = [] |
    singlify  (h :: t) = 
       let val s = singlify t 
       in
           if member (h,t) then s
           else h :: s
       end;

fun remove_one x (h :: t) =
        if  (x=h) then t else h :: (remove_one x t) 
  | remove_one _ [] = [];

fun re_arrangement ([],[])       = true |
    re_arrangement ([],(h :: t)) = false |
    re_arrangement ((h :: t),[]) = false |
    re_arrangement ((h :: t),l2 as (h2 :: t2)) =
        if member (h,l2) then re_arrangement (t,remove_one h l2)
        else false;
                    
fun remove_all_of_one x (h :: t) =
        let val remove_from_tail = remove_all_of_one x t
        in if (x=h) then remove_from_tail
           else h :: remove_from_tail
        end |
    remove_all_of_one _ [] = [];

fun remove_all_of_many (h :: t) l =
        remove_all_of_one h (remove_all_of_many t l) |
    remove_all_of_many  [] l = l;


fun filter_list P []       = [] |
    filter_list P (h :: t) = let val ft = filter_list P t
                             in if P h then (h :: ft)
                                else ft
                             end;

fun pair_lists [] _ = [[]] |
    pair_lists (h :: t) l2 =
        let val hp = map (fn x => (h,x)) l2
            val tp = pair_lists t l2
            fun extend (l) = (map (fn p => p :: l) hp)
        in flat1 (map extend tp)
        end;

   

exception sing_split_nil_set
exception split_nil_set;

infix is_in
infix U
infix section
infix ==
infix mapset
infix within 
infix diff ;

        datatype 'a Set = set of 'a list;
   
        val nil_set = set([]);
        val emptyset = set([]);   (* Alternative name *)

        fun is_nil_set (set([])) = true |
            is_nil_set (set(h :: t)) = false ;
        fun is_empty(s) = is_nil_set(s);    (* Alternative name *)

        fun cardinality (set ([])) = 0 |
            cardinality (set (l as (_ :: _))) = length (singlify l); 
        fun cardinal (set ([])) = 0 |
            cardinal (set (l as (_ :: _))) = length (singlify l); 

        fun singleton e = set ([e]);
        fun is_singleton(s) = (cardinality(s)=1);

        fun  list_set (set ([])) = [] |
             list_set (set (l as (_ :: _))) = singlify l;

        fun (set([])) U s = s |
            (set(h :: t)) U (set(l2)) =
                 if member (h,l2) then
                           (set(t)) U (set(l2))
                 else (set(t)) U (set(h::l2));

        fun add(x,S) = singleton(x) U S;

        fun x is_in (set([])) = false |
            x is_in (set(l as (_ :: _))) = member (x,l);

        fun (set([])) section _ = set([])
          | (set(h :: t)) section s =
                 let val tsecn = set(t) section s
                 in
                     if h is_in s then set([h]) U tsecn
                     else tsecn
                 end;

        fun (set([])) diff _ = set([]) |
            (set(h :: t)) diff s =
                 let val tdiff = set(t) diff s 
                 in
                     if h is_in s then tdiff
                     else set([h]) U tdiff
                 end;

        fun singleton_split (set ([])) = raise sing_split_nil_set |
            singleton_split (set (h :: t)) = (h,set (t));


        fun element_of(s) = let val (x,_) = singleton_split(s) in x end;

        fun split (set ([])) = raise split_nil_set |
            split (set (h :: t)) =
                      let val (a,b) = split_list (h :: t) 
                      in 
                           (set(a),set(b))
                      end;

        fun (set([])) within _ = true |
            (set(h :: t)) within s =
                  (h is_in s)  andalso  (set(t) within s); 
        
        fun s1 == s2 = (s1 within s2) andalso (s2 within s1); 

        fun f mapset (set(l)) = set(map f l); 

        fun set_minus(set(t),x) = set(remove_all_of_one x t);

(*      fun mapset2 (f,set(l,_)) = set(map f l,eq); *)

        fun flatten_set (set(l)) = 
               let fun f (h :: t) = h U (f t) |
                       f []       = nil_set
               in f l
               end;

        fun enumerate show (set(l')) =
               let val l = singlify(l');
                   fun str_list sh [] = "" |
                       str_list sh (h :: []) = sh h |
                       str_list sh (h1 :: (h2 :: t)) =
                                 (sh h1) ^ "," ^ (str_list sh (h2 :: t))
               in "{" ^ (str_list show l) ^ "}"
               end; 



infix 5 *$
fun f *$ S = if is_nil_set S then nil_set
             else let val (e,S1) = singleton_split S
                  in (singleton(f e)) U (f *$ S1)
                  end;

(*  Several simple functions. *)

        exception nilfn;

	fun identity_fn a = a;
	
        fun nil_fn _ = raise nilfn;
        fun emptyfn _ = raise nilfn;

 	fun constant b = fn _ => b ;

	fun f o g = fn x => g(f(x)) ;

        infix **
        fun  f ** (a_set,a_value) =
             if is_nil_set a_set then a_value
             else let val (a,S) = singleton_split a_set
                  in f ** (S,f(a,a_value))
                  end;

	infix 3 X 
	fun  set1 X set2  =
            if is_nil_set set1 then nil_set
            else if is_nil_set set2 then nil_set
                 else let val (a,set3) = singleton_split set1
                          val s = (fn x => (a,x)) mapset set2
                      in s U (set3 X set2)
                      end;



        (*  multicompose : (num # 'a # ('a # 'a -> 'a) # 'a) -> 'a *)
(*        fun multicompose(zero,_,_,init) = init | *)
(*         multicompose(succ(n),F,comp,i) = comp(F,multicompose(n,F,comp,i)); *)


	(*   first :  (alpha*beta) -> alpha *)
	(*   second : (alpha*beta) -> beta *)

   	fun first(x,y)  = x;
	fun second(x,y) = y;

        infix filtered_by
        fun A filtered_by P =
                if is_nil_set A then nil_set
                else let val A_list = list_set A
                     in set (filter_list P A_list)
                     end;

        fun forall(S,p) = is_empty(S filtered_by (fn x => not(p x)));

        exception pairlist_to_fn;

        fun pair_list_to_fn  [] = nil_fn |
            pair_list_to_fn  (l as (_ :: _)) =
                  let fun  pair_off [] _ = raise pairlist_to_fn |
                           pair_off ((a,r) :: t) x = if  (x=a) then r
                                                        else pair_off t x
                  in pair_off l
                  end;

        fun powerset(S) = if is_empty(S) then singleton(S) else
                           let val (s,S') = singleton_split(S) 
                               val PS' = powerset(S') in
                           (add mapset (singleton(s) X PS')) U PS' end;

(* end *)

