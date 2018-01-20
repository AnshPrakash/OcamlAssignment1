type alphabet=A of char;;
type marker=Mark of int;;

type astar=
		Emp
	|  C of alphabet
	|  Str  of marker*astar array;;

exception NullPointer;;
exception Empty;;
exception AtLast;;
exception AtFirst;;
exception TooShort;;

let create s=
	let rec f ar i=
	if(i<(String.length s)) then begin
		ar.(i) <-C (A (s.[i]));
		f ar (i+1)
	end
	else begin
		ar.(i)<-Emp ;
		ar
	end
	in
	let arr=Array.make ((String.length s)+1) Emp 
	in
	let arrayy=f arr 0
	in
	Str((Mark 0),arrayy)
	;;

let lgh s =
		match s with
	| 	Emp ->0
	| 	Str(m,a)->((Array.length a)-1);;

let toString s=
	let rec convert d i str=
		match d with
	| Emp->raise Empty
	|  Str(a,b)-> match b.(i) with
				| C (A u)-> convert d (i+1) (str^(String.make 1 u)) 
				|Emp ->str
	in
	convert s 0 "";;


let concat s1 s2=
	let s = (toString s1)^ (toString s2) in
	create s;;

let nonempty s=
	match s with
	|Str(a,b)->if (((Array.length b)-1)=0) then
					false
				else 
					true
	|_->raise Empty
;;

let reverse s=
	let rec rev s' r i=
		if ((String.length s')=i) then r
		else  rev s' ((Char.escaped s'.[i])^r) (i+1) in
	create (rev (toString s) "" 0)
;;

let first s =
	match s with
	|Emp->raise Empty
	|Str(a,b)->if (b.(0)=Emp) then raise Empty
				else b.(0)
;;

let last s=
	match s with
	|Emp->raise Empty
	|Str(a,b)->if (b.(0)=Emp) then raise Empty
				else b.((Array.length b)-2)
;;

let forward s=
	match s with 
	| Str (m,b)-> match m with
				| Mark a -> if  (((Array.length b)<>1) && ((a+1)<((Array.length b)-1))) then
								Str(Mark (a+1),b)
							else
								raise AtLast

;;

let back s=
	match s with 
	| Str (m,b)-> match m with
				| Mark a -> if (((Array.length b)<>1) && (a-1)>=0) then
								Str(Mark (a-1),b)
							else
								raise AtFirst
;;

let moveTo s n=	
	match s with 
	| Str (m,b)-> match m with
				| Mark a -> if (((Array.length b)<>1) && n>=0 && n<((Array.length b)-1)) then
								Str(Mark n,b)
							else
								raise TooShort
;;

let replace s w=		
		match s with	
	|  Str(m,b)->  if((Array.length b)==1) then
						raise Empty
					else
						match m with
				  	| Mark i -> b.(i)<- C (A w)

;;
