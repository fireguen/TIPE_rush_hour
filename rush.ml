(*TIPE APirot + GRoussel 31/03/26 fonction d'affichage d'un plateau pour premiers tests*)



(*-----------------------------------------------------------------*)
(*------------------------------ Type -----------------------------*)
(*-----------------------------------------------------------------*)

type coord = {mutable x:int;mutable y:int} 
(* coordonees (0,0) en haut a gauche*)

type ide = Rouge|Autre of int

type voiture= {id:ide;taille:int;hor:bool; emp:coord} 
(* emp = coordonée le plus en haut a gauche du vehicule*)


type direction = Droite|Gauche|Haut|Bas|Immobile
type plateau = {dim : int*int ; mutable vlist: voiture list }

type arbre = Noeud of int * arbre list
type 'a file = { mutable entree:'a list; mutable sortie:'a list}


(*-----------------------------------------------------------------*)
(*-------------------------- Exeption -----------------------------*)
(*-----------------------------------------------------------------*)

exception Invalid_movement

exception Trouve of arbre

(*-----------------------------------------------------------------*)
(*----------------------------- Arbre -----------------------------*)
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

  let rec enleve_list (valeur:int) (liste:arbre list):unit=
    (*trouve le noeud de la liste liste s'il est present failwith sinon*)
    match liste with
      |[]-> ()
      |(Noeud (info, fils))::reste-> if info=valeur then raise (Trouve (Noeud(info, fils))) else enleve_list valeur reste
  
  in
  match arb with
  | Noeud(v, fils) -> if v=valeur then arb 
                      else begin 
                        try enleve_list valeur fils ; failwith "l'element n'est pas present parmis les fils directes" 
                        with | Trouve a -> a
                      end

  
(*-----------------------------------------------------------------*)
(*----------------------------- File ------------------------------*)
(*-----------------------------------------------------------------*)

let creer_file ():'a file=
  (* creer un file vide*)  
  {entree=[]; sortie=[]}

let est_vide (fi:'a file):bool= 
  (*indique si fi est vide*)
  fi = creer_file()

let enfile (fi:'a file) (el:'a):unit = 
  (* ajoute el a fi *)
  fi.entree <- el::fi.entree

let rec defile (fi:'a file): 'a =
  (*enleve le dernier element de la file et le renvoie si la file n'est pas vide*)
  match fi.entree, fi.sortie with
    |[],[]-> failwith "votre file est vide"
    |entre, []-> fi.sortie <- List.rev entre ; fi.entree <- [] ; defile fi
    |_,a::b -> fi.sortie <- b ; a


let file_mem (a:'a) (fi:'a file):bool = List.mem a fi.entree || List.mem a fi.sortie


(*-----------------------------------------------------------------*)
(*----------------------------- Gestion ---------------------------*)
(*-----------------------------------------------------------------*)


let creer_voiture (i:ide) (t:int) (h:bool) (ixe:int) (i_grec:int):voiture =
  (* if (i=Autre 0) then failwith"Identifiant 0 interdit" else*)
   {id=i; taille=t; hor=h; emp={x=ixe; y=i_grec}}


let creer_plateau (l:int)(h:int):plateau = 
  (*Cree un plateau de taille l par h contenant une liste de voiture vide*)
  {dim = (l,h); vlist=[]}

let ajouter_voiture (p:plateau) (v:voiture):unit=
  p.vlist <- v::p.vlist


let dupliquer_voiture (v1:voiture):voiture=
  (* duplique v1*)
    {id=v1.id; taille=v1.taille; hor=v1.hor; emp={x=v1.emp.x; y=v1.emp.y}}

    
let dupliquer_plateau (p:plateau):plateau =
  {dim=p.dim; vlist=List.map (fun x -> dupliquer_voiture(x)) p.vlist}


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
  else if not v1.hor && v2.hor then (v2.emp.x+v2.taille-1)>=v1.emp.x && v1.emp.x>=v2.emp.x && ( v1.emp.y+v1.taille-1)>=v2.emp.y && v2.emp.y>=v1.emp.y
  
  (* si v1 horizontale et v2 verticale*)
  else (v1.emp.x+v1.taille-1)>=v2.emp.x && v2.emp.x>=v1.emp.x && ( v2.emp.y+v2.taille-1)>=v1.emp.y && v1.emp.y>=v2.emp.y

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
    | _ -> raise Invalid_movement 


let collision (plat:plateau) (id:ide) (dir:direction) :bool= 
  (* fonction permettant de savoir si la voiture d'identifiant id entre en collision avec une autre voiture presente sur le 
  plateau apres s'etre deplacer de dir. Cette fonction ne deplace pas la voiture en question
  renvois true si la voiture est en contact et false sinon
  
  Verifie egalement si la voiture v entre en collision avec un mur en effectuant le deplacement en direction dir            *)

  let rec collision_dev (vlist: voiture list) (v1:voiture):bool=
    match vlist with
    | voit::queue -> if voit.id <> v1.id then toucher voit v1 || collision_dev queue v1 else collision_dev queue v1
    | _-> false (* si liste vide ou comparaison avec sois-meme*)
  
  in 
  let v = trouve_voiture plat id in 
  let h,l = plat.dim in  
  let v1 = dupliquer_voiture v in (deplacer_v v1 dir; (v1.hor && (v1.emp.x < 0 || v1.emp.x+v1.taille-1 >= h)) || ((not v1.hor) && (v1.emp.y < 0 || v1.emp.y+v1.taille-1 >= l)) || (collision_dev plat.vlist v1))
  



(*--------------------------------------------------------------------*)
(*------------------------------ Robot  ------------------------------*)
(*--------------------------------------------------------------------*)

let rec remove_all tbl key =
  (* Vide entierement une case du dico tbl *)
  if Hashtbl.mem tbl key then (
    Hashtbl.remove tbl key;
    remove_all tbl key
  )   

let rec power (a:int) (n:int):int =
  (* Exponentiation rapide (a^n) *)
  if n = 0 then 1
  else let b = power a (n/2) in
  if n mod 2 = 0 then b * b
  else a * b * b


let id_to_int (id : ide): int =
  match id with
  | Rouge -> 0
  | Autre a -> a

    
let plat_to_int (plat:plateau):int = 
  (* permet de transformer plat en un entier en fonction du nombre de voitures, de leurs positions et de la taille de plat.
  Chaque entier asssocié à une configuration de plateau ayant un nombre de voiture fixée est unique *)
  let t_plat1,t_plat2 = plat.dim 
  in
  let rec voiture_to_int (v_list : voiture list) (t_plat : int) (valeur_hach : int) : int =
    match v_list with
    | [] -> valeur_hach
    | v :: reste -> (* si voiture v horizontale -> ajoute <position en x>*(taille_plateau)^a ou a = 0 si Rouge *)
                        let a = id_to_int v.id in 
                        if (v.hor) then voiture_to_int reste t_plat (valeur_hach + v.emp.x*(power t_plat a))
                    (* si voiture v verticale -> ajoute <position en y>*(taille_plateau)^a ou a = 0 si Rouge *)
                        else voiture_to_int reste t_plat (valeur_hach + v.emp.y*(power t_plat a))
  in voiture_to_int plat.vlist (max t_plat1 t_plat2) 0 

let recupere_int(taille:int)(voit_int):int*int=
  (* recupere et enleve le premier bise en base taille de voit_int*)
  let res = voit_int mod taille in
  let rest_voit_int = (voit_int-res)/taille in
  (rest_voit_int,res)

let int_to_id(vale:int):ide=
  (*rouge si vale =0, (Autre vale) sinon*)
  if vale=0 then Rouge
  else (Autre vale)

let teleporte_voiture (voit:voiture)(pos:int):unit=
  if voit.hor then
      voit.emp.x<-pos
  else
      voit.emp.y<-pos

let rec teleporte_voiture_list (voits:voiture list)(id_recherche:ide)(position:int):unit=
  match voits with
  |voit_courant::reste-> if voit_courant.id =id_recherche then teleporte_voiture voit_courant position
  |[] -> raise (Invalid_argument "la voiture recherche n'est pas sur le plateau")

    
let int_to_plat(pos_init:plateau)(voit_int:int):plateau=
  let rec plateau_from_int(pos_init:plateau)(voit_int:int)(nb:int):unit=
    (* convertit un plateau sous forme d'int a un plateau de type plateau*)
    let taille1, taille2 = pos_init.dim in
    let size = (max taille1 taille2) in 
    let reste, vale = recupere_int size voit_int in
    let id_recherche = int_to_id nb in
    if (vale, reste)<>(0,0) then (
      teleporte_voiture_list   pos_init.vlist    id_recherche vale;
      plateau_from_int pos_init reste (nb+1))
    else 
    ()
      

  in
  let res = dupliquer_plateau pos_init in
  plateau_from_int res voit_int 0;
  res


let rec aux_construit_file_enfant (pf_parent : plateau file) (tpf_p : plateau file)(pf_enfant : plateau file) : plateau file = 
  (*    Construit une file de toutes les positions atteignables legalement (en 1 mouvement)
         a partir de toutes les positions parentes fournient dans la file parent               *)
  if (est_vide pf_parent) then 
    (pf_parent.entree <- tpf_p.entree;      (*     Reconstruction     *)
     pf_parent.sortie <- tpf_p.sortie ;     (*      de pf_parent      *)
     pf_enfant)                        (* Cas de base ou pf_parent est vide *)
  else 
    let p = defile pf_parent in  (* Selection du prochain plateau dont on va construire les enfants *)
    let rec ajoute_file_enfants_p (vl : voiture list): unit = 
      (* Ajoute toutes les positions atteignables legalement en 1 mouvement de la position observee a la file des enfants *)
      match vl with 
      | [] -> ()
      | v :: tvl ->(if v.hor then
                      (if not (collision p v.id Gauche) then 
                        let p1 = dupliquer_plateau p in let v1 = trouve_voiture p1 v.id in (deplacer_v v1 Gauche ; enfile pf_enfant p1 )
                      else ();
                      if not (collision p v.id Droite) then 
                        let p1 = dupliquer_plateau p in let v1 = trouve_voiture p1 v.id in (deplacer_v v1 Droite ; enfile pf_enfant p1 )
                      else ();)

                    else
                      (if not (collision p v.id Haut) then 
                        let p1 = dupliquer_plateau p in let v1 = trouve_voiture p1 v.id in (deplacer_v v1 Haut ; enfile pf_enfant p1 )
                      else ();
                      if not (collision p v.id Bas) then 
                        let p1 = dupliquer_plateau p in let v1 = trouve_voiture p1 v.id in (deplacer_v v1 Bas ; enfile pf_enfant p1 )
                      else ();)

                    ; ajoute_file_enfants_p tvl)
    in (ajoute_file_enfants_p p.vlist; enfile tpf_p p; aux_construit_file_enfant pf_parent tpf_p  pf_enfant)




let rec construit_file_enfant (pf_parent : plateau file) : plateau file = 
  aux_construit_file_enfant ( pf_parent ) (creer_file () ) (creer_file () )







let rec aux_1er_essai_rbt (pf_parent : plateau file)(af_parent : arbre file) (pf_enfant : plateau file)(af_enfant : arbre file) (gen_cour : int list) (dim_p : int) (dico : (int,int) Hashtbl.t): plateau file * arbre file = 
  (* Construit une file de toutes les positions atteignables legalement (en 1 mouvement) a partir de toutes les positions parentes
    fournies dans la file parent sans repetitions de celles-ci ou de retour sur une generation precedente si il y en avait une.

    !!!!!! ATTENTION !!!!!! : pf_parent et af_parent detruits (= vides) durant l'operation                                               *)



  if (est_vide pf_parent) then (pf_enfant,af_enfant)   (* Cas de base ou pf_parent est vide => Renvoi des files construites *)
  else 
    let p , ap = defile pf_parent , defile af_parent in (* Selection du prochain plateau dont on va construire les enfants et de son arbre associé *)
    let intp = plat_to_int p in                         (* Equivalent en entier de p grace a la bijection plat_to_int *)
    let rec construit_enfants_p (vl : voiture list)(gen_cour : int list): int list = 
      (* Ajoute toutes les positions atteignables legalement en 1 mouvement de la position observee a la file des enfants *)
      match vl with 
      | [] -> gen_cour     (* Renvoi de la liste des positions crees pour l'etape suivante avec un nouveau plateau de la file parent *)
      | v :: tvl ->(begin
                    let decalage = power dim_p (id_to_int v.id) in (* Representation absolue sous forme d'entier des deplacements possibles de la voiture v *)
                    let new_gen_cour = ref (gen_cour) in           (* Variable permettant l'actualisation de la liste des plateau de la generation en cours de creation *)
                    let frmoins , frplus = (List.mem (intp - decalage) !new_gen_cour) , (List.mem (intp + decalage) !new_gen_cour) in
                    (* Verfication de la presence du plateau obtenu a l'aide d'un deplacement vers la Gauche/Haut (resp Droite/Bas) dans gen_cour
                       Et dans ce cas, ajout de p en tant que potentiel parent de ces positions. (Utilisation de variables pour reutilisation) *)
                    (if frmoins then Hashtbl.add  dico (intp - decalage) intp;
                    if frplus then Hashtbl.add  dico (intp + decalage) intp;
                    
                    if v.hor then 
                      (* Test de deplacements en cas de deplacement horizontal de la voiture *)
                      (if not (frmoins || List.mem (intp - decalage) (Hashtbl.find_all dico intp) || collision p v.id Gauche) then 
                        (* Interdiction de coups remontants aux "grands-parents" + Test collisions Gauche avec les autres voitures         *)
                        let p1 = dupliquer_plateau p in let v1 = trouve_voiture p1 v.id in 
                          (deplacer_v v1 Gauche ;                              (* Creation plateau enfant apres un coup car coup valide    *)
                          enfile pf_enfant p1 ;                                (* Ajout de l'enfant a la file resultat                     *)
                          new_gen_cour := (intp - decalage) :: !new_gen_cour ; (* Ajout plateau enfant a liste enfants generation actuelle *)
                          Hashtbl.add  dico (intp - decalage) intp;            (* Ajout de p en tant que parent de cette position          *)

                          let na = new_arbre (intp - decalage) in (enfile af_enfant na(*; ajouter_noeud na ap*))) (* Ajout du noeud associe a plateau enfant a la file af_enfant *)

                        else ();

                      if not (frplus || List.mem (intp + decalage) (Hashtbl.find_all dico intp) || collision p v.id Droite) then 
                        (* Interdiction de coups faisant retourner en arriere + Test collisions Droite avec les autres voitures*)
                        let p1 = dupliquer_plateau p in let v1 = trouve_voiture p1 v.id in 
                          (deplacer_v v1 Droite ;                              (* Creation plateau enfant apres un coup car coup valide    *)
                          enfile pf_enfant p1 ;                                (* Ajout de l'enfant a la file resultat                     *)
                          new_gen_cour := (intp + decalage) :: !new_gen_cour ; (* Ajout plateau enfant a liste enfants generation actuelle *)
                          Hashtbl.add  dico (intp + decalage) intp;            (* Ajout de p en tant que parent de cette position          *)

                          let na = new_arbre (intp + decalage) in (enfile af_enfant na(*; ajouter_noeud na ap*))) (* Ajout du noeud associe a plateau enfant a la file af_enfant *)

                      else ();)

                    else

                      (* Test de deplacements en cas de deplacement vertical de la voiture *)
                      (if not (frmoins || List.mem (intp - decalage) (Hashtbl.find_all dico intp) || collision p v.id Haut) then 
                        (* Interdiction de coups faisant retourner en arriere + Test collisions Haut avec les autres voitures*)
                        let p1 = dupliquer_plateau p in let v1 = trouve_voiture p1 v.id in 
                          (deplacer_v v1 Haut ;                                (* Creation plateau enfant apres un coup car coup valide    *)
                          enfile pf_enfant p1 ;                                (* Ajout de l'enfant a la file resultat                     *)
                          new_gen_cour := (intp - decalage) :: !new_gen_cour ; (* Ajout plateau enfant a liste enfants generation actuelle *)
                          Hashtbl.add  dico (intp - decalage) intp;            (* Ajout de p en tant que parent de cette position          *)

                          let na = new_arbre (intp - decalage) in (enfile af_enfant na(*; ajouter_noeud na ap*))) (* Ajout du noeud associe a plateau enfant a la file af_enfant *)

                        else ();

                        if not (frplus || List.mem (intp + decalage) (Hashtbl.find_all dico intp) || collision p v.id Bas) then 
                        (* Interdiction de coups faisant retourner en arriere + Test collisions Bas avec les autres voitures*)
                        let p1 = dupliquer_plateau p in let v1 = trouve_voiture p1 v.id in 
                          (deplacer_v v1 Bas ;                                 (* Creation plateau enfant apres un coup car coup valide    *)
                          enfile pf_enfant p1 ;                                (* Ajout de l'enfant a la file resultat                     *)
                          new_gen_cour := (intp + decalage) :: !new_gen_cour ; (* Ajout plateau enfant a liste enfants generation actuelle *)
                          Hashtbl.add  dico (intp + decalage) intp;             (* Ajout de p en tant que parent de cette position          *)
                          print_int (intp+decalage);
                          print_newline ();
                          let na = new_arbre (intp + decalage) in (enfile af_enfant na(*; ajouter_noeud na ap*)))

                      else ();)

                    ; construit_enfants_p tvl !new_gen_cour)  end)

                  in let new_gen_cour = construit_enfants_p p.vlist gen_cour in (* Construction des enfants de p *)
                  (remove_all dico intp;                      (* Liberation de l'espace de stockage du dictionnaire *) 
                  aux_1er_essai_rbt pf_parent af_parent pf_enfant af_enfant new_gen_cour dim_p dico)  
                  (* Nouvel appel de la fonction sur le plateau suivant de la file tout en 
                    conservant les informations recuperes durant les precedentes iterations *)

let rec _1er_essai_rbt (pf_parent : plateau file)(af_parent : arbre file) (dim_p : int) (dico : (int,int) Hashtbl.t): plateau file * arbre file = 
  (* Construit une file de toutes les positions atteignables legalement (en 1 mouvement) a partir de toutes les positions parentes
    fournies dans la file parent sans repetitions de celles-ci ou de retour sur une generation precedente si il y en avait une.

    !!!!!! ATTENTION !!!!!! : pf_parent et af_parent detruits (= vides) durant l'operation                                               *)

  aux_1er_essai_rbt ( pf_parent ) ( af_parent ) (creer_file () ) (creer_file () ) ( [] ) ( dim_p ) ( dico )




(*

let rec enfants_of_p (pf : plateau file) (p : plateau) (parbre : arbre) (enfantsl : int file): int file = 
    match p.vlist with
    | [] -> enfantsl
    | v::tvl -> let idv = match v.ide with |Rouge -> 0 |Autre x -> x in
                let Noeud n al = parbre in 
                  if not (collision p v.ide Bas || collision p v.ide Droite) then 


let recherche_solution (p : plateau) : int list * arbre = 
  let t_plat1,t_plat2 = plat.dim
  in
  let intp , t_plat = (plat_to_int p ), max t_plat1 t_plat2
  in
  
*)


(*--------------------------------------------------------------------*)
(*----------------------- Interface Utilisateur ----------------------*)
(*--------------------------------------------------------------------*)

    
let plateau_vers_matrice (p: plateau):ide array array =
  (*Transforme un plateau en matrice pour l'impression ecran et le retour visuel*)
  let v1 = creer_voiture (Autre 0) (0) (true) (0) (0) in

  let dim1,dim2 = p.dim in

  let rec placer_voiture_dev (vlist: voiture list) (v:voiture) (mat: ide array array):ide array array =
    (*Place les differentes voitures du plateau stockees dans vlist dans une matrice mat. v est la voiture a 
      placer et elle est placee entierement dans la matrice avant une seconde iteration de placer-voiture-dev. *)
      for i = 0 to (v.taille-1) do 
        if v.hor then mat.(v.emp.y).(v.emp.x + (i)) <- v.id     (* Place la voiture par case    *)
        else mat.(v.emp.y + (i)).(v.emp.x) <- v.id              (* qu'elle occupe case par case *)
      done ;
      match vlist with 
      | [] -> mat                                               (* Appel de la fonction de nouveau *) 
      | v2 :: tvlist -> placer_voiture_dev tvlist v2 mat        (* avec la voiture suivante        *)
    in placer_voiture_dev (p.vlist) v1 (Array.make_matrix dim1 dim2 (Autre 0))   (* Appel initial avec une matrice et 
                                                                              une voiture vide (taille 0)      *)

                                                                              

(*--------------------------------------------------------------------*)
(*----------------------------- Affichage ----------------------------*)
(*--------------------------------------------------------------------*)

  let rec print_ligne (n:int):unit = 
    (*Affiche une ligne de 4*n _ *)
    if n = 0 then () 
    else (print_string "_____" ; print_ligne (n-1))
  
  let print_id (i:ide):unit = 
    (*Affiche la valeur associee au type ide en entree*)
    match i with
    | Rouge -> print_string "R "
    | Autre 0 -> print_string "  "
    | Autre x -> (if x < 10 then print_char ' ' else () ; print_int x)
  
  

let affiche_plateau (p1:plateau):unit = 
  print_newline ();
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
  in affiche_lignes_plateau 0;
  print_newline ()














let ()=

(****************************************************************************************************************************)
(*                                                                                                                          *)
  let nombre_de_generations_a_afficher = 6 in        (* Nombre de generation de plateaux a afficher (0 = position initiale) *)
  let dim_t = 6 in                                   (* Dimension du plateau de la position initiale si celui-ci est carre  *)
  let t = creer_plateau dim_t dim_t in               (* Position initiale vide. Remplir avec les voitures ci-apres si voulu *)
  let v = creer_voiture Rouge 2 true 3 2 in          (* Voiture Rouge. Par defaut placee horizontale sur la deuxieme ligne  *)
  let v2 = creer_voiture (Autre 3) 2 true 0 2 in     (*    Voiture horizontale ( taille 2 ) d'identifiant 3  ( max 18 )     *)
  let v4 = creer_voiture (Autre 5) 2 false 2 0 in    (*    Voiture vertical    ( taille 2 ) d'identifiant 5  ( max 18 )     *)
  let v3 = creer_voiture (Autre 12) 3 true 2 2 in    (*    Camion horizontale  ( taille 3 ) d'identifiant 12 ( max 18 )     *)
  let v5 = creer_voiture (Autre 18) 3 false 2 0 in   (*    Camion vertical     ( taille 3 ) d'identifiant 18 ( max 18 )     *)
(*                                                                                                                          *)
(****************************************************************************************************************************)

  let dico = Hashtbl.create 10 in
  let refill_file = creer_file () in

  (ajouter_voiture t v2;
  ajouter_voiture t v3; 
  ajouter_voiture t v;
  ajouter_voiture t v4;
  
  let arb = Noeud (plat_to_int t,[]) in
  
  
  (print_newline ();
  print_string "#####################################################################################################################################";
  print_newline();
  let f = ref {entree = [t]; sortie = []} in 
  let fa = ref {entree = [arb]; sortie = []} in
  
  
  for i = 0 to nombre_de_generations_a_afficher do
    print_newline ();
    print_newline ();
    print_string "Gen "; print_int i;
    print_newline ();
    print_newline ();
    while not (est_vide !f) do
      let plat = defile !f in
      enfile refill_file plat;
      affiche_plateau plat;
      print_int(plat_to_int plat);
      print_newline ()
    done;
    while not (est_vide refill_file) do
      let plat = defile refill_file in
      enfile !f plat;
    done;

    let ffa = _1er_essai_rbt !f !fa dim_t dico in match ffa with |(a,b)-> (f:=a;fa:=b)

  done))



