(*Mikolaj Grzywacz 
CR: Piotr Wojtczak*)

type point = float * float

type kartka = point -> int

(*zwraca (a, b), gdzie a to wspolczynnik kierunkowy prostej, a b wyraz wolny*)
(* (float * float) -> (float * float) -> (float * float) *)
let prosta (p1x, p1y) (p2x, p2y) =
	if p1x=p2x then 
		(*I tak nigdy do tego nie dojdzie*)
		failwith "x1=x2" 
	else 
		let a = (p1y -. p2y)/.(p1x -. p2x) in
		let b = p1y -. (a *. p1x) in
	(a, b)

(*zwraca (x, y) bedacy wspolzednymi odbitego punktu*)
(* p1, p2 wyznaczja prosta (x,y) to punkt do odbicia*)
(* (float * float) -> (float * float) -> (float * float) -> (float * float) *)
let odbicie (p1x, p1y) (p2x, p2y) (x, y) =
	if p1x = p2x then
		(* Prosta pionowa *)
		if p1y = p2y then failwith "Punkty maja byc rozne"
		else (2.*.p1x -. x, y)
	else let (a, b) = prosta (p1x, p1y) (p2x, p2y) in
	(* Odbicie punktu wzgledem prostej *)
	(x*.(1.-.a*.a)/.(1.+.a*.a)+.((y-.b)*.2.*.a/.(1.+.a*.a)),
		x*.2.*.a/.(1.+.a*.a) -. ((y-.b)*.(1.-.a*.a)/.(1.+.a*.a)) +. b)

(*zwraca po ktorej stronie prostej lezy punkt (x, y) (patrzac od p1 do p2)*)
(* wartosc dodatnia jak po lewej, 0 jak na prostej, ujemna jak po prawej*)
(* (float * float) -> (float * float) -> (float * float) -> float *)
let ktora_strona (p1x, p1y) (p2x, p2y) (x, y) =
	(y -. p1y)*.(p2x -. p1x) -. (p2y -. p1y)*.(x -. p1x)
	
(* point -> point -> kartka *)
let prostokat (x1,y1) (x2,y2) =
	fun (a,b) -> 
		if a >= x1 && a <= x2 && b >= y1 && b <= y2 then 1
		else 0
		
(* point -> float -> kartka *)
let kolko (x1,y1) r =
	fun (a,b) -> 
		let odlx = (a -. x1)*.(a -. x1) in
		let odly = (b -. y1)*.(b -. y1) in
		if (odlx +. odly) <= (r *. r) then 1
		else 0

(* point -> point -> kartka -> kartka *)
let zloz (x1, y1) (x2, y2) kartka = 
	fun punkt -> 
		let strona = ktora_strona (x1, y1) (x2, y2) punkt in	
		if strona > 0. then 
			kartka punkt + kartka (odbicie (x1, y1) (x2, y2) punkt)
		else
			if strona = 0. || strona = (-0.) then
				kartka punkt
			else 
				0

(* (point * point) list -> kartka -> kartka *)
let skladaj lista kartka =
	List.fold_left (fun acc p -> zloz (fst p) (snd p) acc) kartka lista 
	