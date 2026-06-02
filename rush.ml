(*TIPE APirot + GRoussel 31/03/26 fonction d'affichage d'un plateau pour premiers tests*)



(*-----------------------------------------------------------------*)
(*------------------------------ Type -----------------------------*)
(*-----------------------------------------------------------------*)

type coord = {mutable x:int;mutable y:int} (* coordonees (0,0) en haut a gauche*)
type ide = Rouge|Autre of int
type voiture= {id:ide;taille:int;hor:bool;mutable emp:coord} (* emp = coordonée le plus en haut a gauche du vehicule*)
type direction = Droite|Gauche|Haut|Bas|Immobile
type plateau = {dim : int*int ; mutable vlist: voiture list }
type arbre = Noeud of int * arbre list
type 'a file = {  entree:'a list; sortie:'a list}


(*-----------------------------------------------------------------*)
(*------------------------------arbre------------------------------*)
(*-----------------------------------------------------------------*)

let new_arbre (valeur:int):arbre =
  (* cree un nouvelle arbre qui a comme valeur de noeud valeur*)
  Noeud (valeur,[])

let ajouter_noeud (nouveau:arbre) (arb:arbre):arbre=
  (*ajoute nouveau aux fils de arb*)
  match arb with
  |Noeud (x, liste) ->Noeud (x, nouveau::liste)

let find_fils (valeur:int) (arb:arbre):arbre=
  (* recherche le noeud des fils directes de arb s'il est present*)

  let rec enleve_list (valeur:int) (liste:arbre list):arbre=
    (*trouve le noeud de la liste liste s'il est present failwith sinon*)
    match liste with
      |[]-> failwith "l'element n'est pas present parmis les fils directes"
      |(Noeud (info, fils))::reste-> if info=valeur then Noeud(info, fils) else enleve_list valeur reste
  
  in
  match arb with
  | Noeud(valeur, fils) -> enleve_list valeur fils

  
(*-----------------------------------------------------------------*)
(*------------------------------file-------------------------------*)
(*-----------------------------------------------------------------*)

let creer_file ():'a file=
  (* creer un file vide*)  
  {entree=[]; sortie=[]}

let est_vide (fi:'a file):bool= 
  (*indique si fi est vide*)
  fi = creer_file()

let enfile (fi:'a file) (el:'a):'a file=
  (* ajoute el a fi *)
  {entree=el::fi.entree; sortie=fi.sortie}

let rec defile (fi:'a file): 'a*'a file=
  (*enleve le dernier element de la file et le renvois si la file n'est pas vide*)
  match fi.entree, fi.sortie with
    |[],[]-> failwith "votre pile est vide"
    |entre, []-> defile {entree=[];sortie= List.rev (entre)}
    |_,a::b -> (a,{entree=fi.entree;sortie=b})








(*-----------------------------------------------------------------*)
(*----------------------------- Gestion ---------------------------*)
(*-----------------------------------------------------------------*)



let dupliquer_voiture (v1:voiture):voiture=
  (* duplique v1*)
    {id=v1.id; taille=v1.taille; hor=v1.hor; emp={x=v1.emp.x; y=v1.emp.y}}

                     
let toucher (v1:voiture) (v2:voiture):bool =
  (*indique si v1 et v2 se touche 
  une voiture se touche elle même*)

  if v1.id=v2.id then true
  (* disjonction de cas en fonction de l'orientation des voitures*)
  
  (* si les deux voitures sont horizontale*)
  else if v1.hor && v2.hor then v1.emp.y=v2.emp.y && ( v2.emp.x+v2.taille-1)>=v1.emp.x && (v2.emp.x<=(v1.emp.x+v1.taille-1))
  
  (* si les deux voitures sont verticales*)
  else if not v1.hor && not v2.hor then v2.emp.x=v1.emp.x && v1.emp.y<=(v2.emp.y+v2.taille-1) && v2.emp.y<=(v1.emp.y+v1.taille-1)

  (* si v1 verticale et v2 horizontale*)
  else if not v1.hor && v2.hor then (v2.emp.x+v2.taille-1)>=v1.emp.x && v1.emp.x>=v2.emp.x && ( v2.emp.y+v2.taille-1)>=v1.emp.y && (v2.emp.y<=(v1.emp.y+v1.taille-1))
  
  (* si v1 horizontale et v2 verticale*)
  else (v1.emp.x+v1.taille-1)>=v2.emp.x && v2.emp.x>=v1.emp.x && ( v1.emp.y+v1.taille-1)>=v2.emp.y && (v1.emp.y<=(v2.emp.y+v2.taille-1))

let trouve_voiture (plat:plateau) (id:ide):voiture=
    (* trouve la voiture qui a l'id id dans plateau*)
    let rec trouve_voiture_dev (vl: voiture list) (id:ide)=
        match vl with
        | []-> failwith " il n'existe pas de voiture avec un tel id sur le plateau"
        | a::b -> if a.id=id then a 
                  else trouve_voiture_dev b id
                in trouve_voiture_dev plat.vlist id






let deplacer_v (voit:voiture) (d:direction):unit=
  (* permet de deplacer la voiture v d'un case dans la direction correspond a d *)
    match d with
    |Droite when voit.hor -> voit.emp.x<-voit.emp.x+1
    |Gauche when voit.hor -> voit.emp.x<-voit.emp.x-1
    |Haut when not voit.hor -> voit.emp.y<-voit.emp.y-1
    |Bas when not voit.hor -> voit.emp.y<-voit.emp.y+1
    |Immobile -> ()
    |_ -> failwith "la direction est pas bonne !!!!!!!!!!!!!!!"


let collision (plat:plateau)(id:ide) (dir:direction) :bool= 
  (* fonction permettant de savoir si la voiture d'identifiant id entre en collision avec une autre voiture presente sur le 
  plateau apres s'etre deplacer de dir. Cette fonction ne deplace pas la voiture en question
  renvois true si la voiture est en contact et false sinon*)
  let rec collision_dev (vlist: voiture list) (v1:voiture):bool=
    match vlist with
    | voit::queue when voit.id!= v1.id-> toucher voit v1 || collision_dev queue v1
    | _-> false (* si liste vide ou comparaison avec sois-meme*)
  
  in let v = trouve_voiture plat id in let v1=dupliquer_voiture v in (deplacer_v v1 dir; collision_dev plat.vlist v1)
  



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

(*--------------------------------------------------------------------*)
(*------------------------------ le bot  -----------------------------*)
(*--------------------------------------------------------------------*)


(*
let recherche_solution (p : plateau) : int list * arbre = 
  let rec aaa (pf : plateau file) (p : plateau)  = 
    match p.vlist with
    | [] -> 
    | v::tvl -> if 
  in
  let rec construit_file_enfant (pf_parent : plateau file) (pf_enfant : plateau file) : plateau file = 
    if (est_vide pf_parent) then pf_enfant 
    else let p = defile pf_parent in 
      
let tab_to_int (plat:plateau):int=
*)




let ()=
  
  let t=creer_plateau 6 6 in 
  (let v=creer_voiture Rouge 2 true 0 0 in
  (ajouter_voiture t v; 
  deplacer_v v Droite;
  affiche_plateau t))



