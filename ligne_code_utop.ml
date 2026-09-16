let p= creer_plateau 6 6;;
let v1=creer_voiture (Rouge) 2 true 0 0;;
ajouter_voiture p v1;;
let v3=creer_voiture (Autre 3) 2 false 0 2;;
ajouter_voiture p v3;;
affiche_plateau p;;
