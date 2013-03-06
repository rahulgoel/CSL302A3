type var = I of int
					| S of string

type  preterm = X of var
								| Node of string* (preterm list)
									
let rec sigok	l1 =
		let rec helper l n =
			match l with 	
			| [] -> false
			| (s1,i1)::rest1 -> if (s1=n) then true 							
											 		else helper rest1 n in
	match l1 with 
			| [] -> true
			| (s,i)::rest ->  if (i<0|| helper rest s) then false
												 else sigok rest 
												
let  wellformedh ptm sigl1 = 
	let rec help str l =
		match l with 
		| [] -> 0
		| (s1,i1)::rest1 -> if( s1 = str) then i1
												else help str rest1 in
 											
	match ptm with
	| X s -> true
	| Node (strg , pret) ->   if ( help strg sigl1 = List.length pret) then true
													else false


let rec wellformed ptm sigl=
	let rec wellformlist ptml l =
		match ptml with
		| [] -> true
		| hd::res -> if (wellformedh hd l) then (wellformlist res l)
									else false in
	match ptm with
	| X s -> true
	| Node (strg,prel) -> if ( wellformedh ptm sigl && wellformlist prel sigl) then true
												else false								


let  rec subst ( ( t : preterm ), s) = 
	let rec helpl ( (list1 : preterm list) ,s1, ( l2 : preterm list ))= match list1 with
													|[] ->  l2
													| hd::res -> helpl (res ,s1 ,  l2@[subst (hd ,s1)]  ); in	
	match s with													
	| (x, y)  ->  match t with 
									|  X S st -> if (x =st) then y
															else t
									|  X I i ->  t
									| Node (str,list) ->Node(str, helpl (list, s, []) )
																													(*List.iter (subst list s)*)																				
	| _ ->  t
exception E
let rec apply sub listterms outl = match listterms with 
																		| [] -> outl
																| hdlist::restlist-> apply sub restlist outl@[subst (hdlist ,sub)]						

let rec applylist sublist listterms = match sublist with
																	| [] -> listterms
																	| hds::res-> applylist res (apply hds listterms [] ) 

let rec remove_at n = function
    | [] -> []
    | h :: t -> if n = 1 then t else if n=0 then h::t else remove_at (n-1) t



let rec mgu t1 t2 listu =
	let rec isx s1 x=
	 let rec chklist list ( str1 : string )  = match list with
									|[] -> false
									|hd::rest-> (isx  str1 hd )||(chklist rest str1) in 
							match x with		
									| X ( S y) -> if (s1=y) then true 
																else false
									| X ( I w) -> false
									| Node(str,list) -> chklist list s1  in

	let rec mgulist list1 list2 listu1 =
		match list1 with
							| []-> listu1
							| hda::rsta->match list2 with
														hda2::rsta2->mgulist rsta (applylist listu1 rsta2 ) listu1@(mgu hda hda2 []) in
									
	match t2 with 
	| X (I i) -> match t1 with 
							| X ( I j) -> if (i =j) then [( "I dont care" ,X( I j) )]
														else  raise E
							| X ( S x) -> listu@[(x,X (I i))]
							| Node(str,list) -> raise E
	| X ( S s) -> match t1 with
							| X ( I j ) -> listu@[(s,X (I j) )]
							| X ( S x) -> listu@[(x, X ( S s) )]
							| Node(str,list) ->if ( (isx s t1) = false)  then listu@[(s, Node(str,list) )]													
																	else raise E
	| Node(str,list) -> match t1 with																			
	            | X ( I j) -> raise E
							| X ( S x) -> if ( (isx s t1) = false)  then listu@[( x,Node(str,list))]	
														else raise E
							|	Node(str1,list1) -> if(str1=str && (List.length list= List.length list1) ) then 
																										(*for i = i to List.length list do*)
																										(* applylist listu list1; *)
																										
																										match   list with
																										| [] -> listu
																										| hd::rest->match  list1 with
																																		|hd2::res2->  listu@(mgu hd hd2 [])@(mgulist rest res2 [])
																											 
																			else raise E							
																																																																																																							 							