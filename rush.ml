(*TIPE APirot + GRoussel 31/03/26 fonction d'affichage d'un plateau pour premiers tests*)



(*-----------------------------------------------------------------*)
(*------------------------------ Type -----------------------------*)
(*-----------------------------------------------------------------*)

type coord = {mutable x:int;mutable y:int}
type ide = Rouge|Autre of int
type voiture= {id:ide;taille:int;hor:bool;mutable emp:coord} (* emp = coordonée le plus en haut a gauche du vehicule*)
type direction = Droite|Gauche|Haut|Bas|Immobile
type plateau = {dim : int*int ; mutable vlist: voiture list }


(*-----------------------------------------------------------------*)
(*----------------------------- Gestion ---------------------------*)
(*-----------------------------------------------------------------*)


(*pas fini*)
let dupliquer_voiture (v1:voiture):voiture=
    {id=v1.ide; taille=v1.taille; hor=v.hor; emp={x=v1.emp.x; y=v1.emp.y}}


let rec toucher_lent (v1:voiture) (v2:voiture) (i:int) (j:int):bool=
  match v1, v2 with
  | {_;t1; h1; {x1; y1} },{_;t2; h2; {x2; y2} } -> if v1.id=v2.id || i>=t1 || j>=t2 then false
                                                  else if h1 && h2 then ((x1+i)=(x2+j) && y1=y2) || (toucher_lent v1 v2 i (j+1))  || (toucher_lent v1 v2 (i+1) j)
                                                  else if h1 && not h2 then ((x1+i)=(x2) && (y1)=(y2+j)) || (toucher_lent v1 v2 i (j+1)) || (toucher_lent v1 v2 (i+1) j)
                                                  else if not h1 && not h2 then ((x1)=(x2) && (y1+i)=(y2+j)) || (toucher_lent v1 v2 i (j+1)) || (toucher_lent v1 v2 (i+1) j)
                                                  else ((x1)=(x2+j) && (y1+i)=(y2)) || (toucher_lent v1 v2 i (j+1)) || (toucher_lent v1 v2 (i+1) j)
                                                      
let toucher (v1:voiture) (v2:voiture):bool =
  if v1.id=v2.id then false
  (* disjonction de cas en fonction de l'orientation des voitures*)
  else if h1&&h2 then v1.coord.y=v2.coord.y && ( v2.coord.x+v2.taille-1)>=v1.coord.x && (v2.coord.x<=(v1.coord.x+taille-1))
  else if not h1 && h2 then v1.coord.x=v2.coord.x && ( v2.coord.y+v2.taille-1)>=v1.coord.y && (v2.coord.y<=(v1.coord.y+taille-1))
  else if not h1 && h2 then v1.coord.y<=v2.coord.y && v2.coord.y<=(v1.coord+taille-1) && v2.coord.x<=v1.coord.x && v1.coord.x<=(v2.coord.x+taille-1)
(*pas fini*)





let collision (vlist: voiture list) (id:ide) (dir:direction) :bool= 
  let v = trouve_voiture vlist id in
  let collision_dev
  match vlist with
    | []-> false
    | voit::queue-> toucher voit v 0 0 || collision queue id dir 


let trouve_voiture (plat:plateau) (id:ide):voiture=
    (* trouve la voiture qui a l'id id dans plateau*)
    let rec trouve_voiture_dev (vl: voiture list) (id:ide)=
        match vl with
        | []-> failwith " il n'existe pas de voiture avec un tel id sur le plateau"
        | a::b -> if a.id=id then a 
                  else trouve_voiture_dev b id
                in trouve_voiture_dev plat.vlist id


let deplacer_v (plat:plateau) (id:ide) (d:direction):unit=
  (* permet de deplacer la voiture v d'un case dans la direction correspond a d *)
  let voit=trouve_voiture plat id in
    match d with
    |Droite when voit.hor -> voit.emp.x<-voit.emp.x+1
    |Gauche when voit.hor -> voit.emp.x<-voit.emp.x-1
    |Haut when not voit.hor -> voit.emp.y<-voit.emp.y-1
    |Bas when not voit.hor -> voit.emp.y<-voit.emp.y+1
    |_ -> failwith "la direction est pas bonne !!!!!!!!!!!!!!!"
  



let creer_voiture (i:ide) (t:int) (h:bool) (ixe:int) (i_grec:int):voiture=
  {id=i; taille=t; hor=h; emp={x=ixe; y=i_grec}}


let creer_plateau (l:int)(h:int):plateau = 
  (*Cree un plateau de taille l par h contenant une liste de voiture vide*)
  {dim = (l,h); vlist=[]}

let ajouter_voiture (p:plateau) (v:voiture):unit=
  p.vlist <- v::p.vlist



(*--------------------------------------------------------------------*)
(*----------------------- Interface Utilisateur ----------------------*)
(*--------------------------------------------------------------------*)

    
let plateau_vers_matrice (p: plateau):ide array array =
  (*Transforme un plateau en matrice pour l'impression ecran et le retour visuel*)
  let v1 = creer_voiture (Autre 0) (0) (true) (0) (0) in

  let rec placer_voiture_dev (vlist: voiture list) (v:voiture) (mat: ide array array):ide array array =
    (*Place les differentes voitures du plateau stockees dans vlist dans une matrice mat. v est la voiture a 
      placer et elle est placee entierement dans la matrice avant uneseconde iteration de placer-voiture-dev. *)
      for i = 0 to (v.taille-1) do 
        if v.hor then mat.(v.emp.y).(v.emp.x + (i)) <- v.id     (* Place la voiture par case    *)
        else mat.(v.emp.y + (i)).(v.emp.x) <- v.id              (* qu'elle occupe case par case *)
      done ;
      match vlist with 
      | [] -> mat                                               (* Appel de la fonction de nouveau *) 
      | v2 :: tvlist -> placer_voiture_dev tvlist v2 mat        (* avec la voiture suivante        *)
      placer_voiture_dev tvlist 
    in placer_voiture_dev (p.vlist) v1 (Array.make_matrix 6 6 (Autre 0))   (* Appel initial avec une matrice et 
                                                                              une voiture vide (taille 0)      *)

                                                                              

(*--------------------------------------------------------------------*)
(*----------------------------- Affichage ----------------------------*)
(*--------------------------------------------------------------------*)

  let rec print_ligne (n:int):unit = 
    (*Affiche une ligne de 4*n _ *)
    if n = 0 then () 
    else (print_string "____" ; print_ligne (n-1))
  
  let print_id (i:ide):unit = 
    (*Affiche la valeur associee au type ide en entree*)
    match i with
    | Rouge -> print_char 'R'
    | Autre 0 -> print_char ' '
    | Autre x -> print_int x
  
  

let affiche_plateau (p1:plateau):unit = 
  (*Affiche le plateau donne avec des cases constituees de _ et de |*)
  let p = plateau_vers_matrice p1 in
  let nh,nv = (Array.length p) , (Array.length (p.(0))) 
  in 
  let rec affiche_lignes_plateau (j:int) :unit = 
    (*Affiche le plateau ligne a ligne*)
    if j = nh then (print_ligne nv; print_newline())
    else
    (print_ligne(nv);
    print_newline ();
    let rec affiche_cases_plateau (i:int):unit =
      (*affiche la ligne contenant les valeurs des cases une par une, laisse une ouverture pour la sortie*)
      if i = nv && j = 2 then print_char ' ' (*cree l'ouverture pour la sortie*)
      else if i = nv then print_char '|' (*Affiche le dernier |*)
      else
      (print_string "| ";
      print_id (p.(j).(i));
      print_char ' ';
      affiche_cases_plateau (i+1))
    in affiche_cases_plateau 0;
    print_newline ();
    affiche_lignes_plateau (j+1))
  in affiche_lignes_plateau 0



let jeu (p: int array array) (v: voiture list)=()

let ()=

  let t=creer_plateau 6 6 in
  (let v=creer_voiture Rouge 2 true 0 0 in
  (ajouter_voiture t v; 
  deplacer_v t v.id Droite;
  affiche_plateau t))



3.14
