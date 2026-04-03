(*TIPE APirot + GRoussel 31/03/26 fonction d'affichage d'un plateau pour premiers tests*)



(*-----------------------------------------------------------------*)
(*------------------------------- Type ----------------------------*)
(*-----------------------------------------------------------------*)

type coord = {mutable x:int;mutable y:int}
type ide = Rouge|Autre of int
type voiture= {id:ide;taille:int;hor:bool;mutable emp:coord} (* emp = coordonée le plus en haut a gauche du vehicule*)
type direction = Droite|Gauche|Haut|Bas|Immobile
type plateau = {dim : int*int ; mutable vlist: voiture list }


(*-----------------------------------------------------------------*)
(*------------------------------- Gestion -------------------------*)
(*-----------------------------------------------------------------*)


let collision (vlist: voiture list) (voit:voiture) (dir:direction) :bool= true


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

(*
let placer_voiture (p: plateau) (v:voiture):unit=
  let rec placer_voiture_dev (p:plateau) (v:voiture) (i:int):unit=
    if i>v.taille then ()
    else
    match v.id with 
    |Rouge -> if v.hor then (p.(v.emp.y).(v.emp.x+(i-1)) <- Rouge;
                              placer_voiture_dev p v (i+1) )

      else (p.(v.emp.y+(i-1) ).(v.emp.x) <- Rouge;
            placer_voiture_dev p v (i+1) )

    |Autre a ->  if v.hor then (p.(v.emp.y).(v.emp.x+(i-1))<- Autre a;
                                placer_voiture_dev p v (i+1) )

      else ( p.(v.emp.y+(i-1) ).(v.emp.x) <- (Autre a);
            placer_voiture_dev p v (i+1) )

  in placer_voiture_dev p v 1
*)


(*-----------------------------------------------------------------*)
(*------------------------------- Affichage -----------------------*)
(*-----------------------------------------------------------------*)

    
let plateau_vers_matrice (p: plateau):ide array array =
  (*Transforme un plateau en matrice pour l'impression ecran et le retour visuel*)
  let v1 = creer_voiture (Autre 0) (0) (true) (0) (0) in

  let rec placer_voiture_dev (vlist: voiture list) (v:voiture) (i:int) (mat: ide array array):ide array array =
    (*Place les differentes voitures du plateau stockees dans vlist dans une matrice mat.
      v est la voiture a placer et i represente la possibilite de placer une partie de la
      voiture dans la case suivante.*)
    match vlist with
    | [] -> mat
    | v2::tvlist -> 
    if i>v.taille then (match tvlist with 
                        | [] when v.id <> v2.id-> placer_voiture_dev [v2] v2 1 mat
                        | _ -> placer_voiture_dev tvlist v2 1 mat)
    else 
      begin
      match v.id with 
      | Rouge -> if v.hor then (mat.(v.emp.y).(v.emp.x+(i-1)) <- Rouge;
                                placer_voiture_dev vlist v (i+1) mat )

                else (mat.(v.emp.y+(i-1) ).(v.emp.x) <- Rouge;
                  placer_voiture_dev vlist v (i+1) mat )

      |Autre a -> if v.hor then (mat.(v.emp.y).(v.emp.x+(i-1)) <- Autre a;
                                  placer_voiture_dev vlist v (i+1) mat )

                  else ( mat.(v.emp.y+(i-1) ).(v.emp.x) <- (Autre a);
                      placer_voiture_dev vlist v (i+1) mat)
      end

    in placer_voiture_dev (p.vlist) v1 1 (Array.make_matrix 6 6 (Autre 0))

(*-----------------------------------------------------------------*)
(*------------------------------- Affichage -----------------------*)
(*-----------------------------------------------------------------*)

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
