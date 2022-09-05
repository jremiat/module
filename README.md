# Application présentant 2 modèles en utilisant les modules


## Fichier


Garder l'app R, les deux scripts contenant les modules dans le même dossier.
Si des images sont affichées, les mettre dans un fiichier "www" 

## Arguments pris par les modules

 Toujours garder les "library" et "source"
 Il faut créer des objets qui seront des arguments donnés aux parties Server du module nommé "modelecoexServer":
 Liste des étiquettes en latex des paramètres, liste des étiquettes des paramètres en UTF8(dans le même ordre), le point de départ de la réaction (mentionnant le nom des compartiments),
 nombre d'injections pour lesquelles les valeurs de paramètres sont estimées la liste de toutes les valeurs des paramètres (dans le même ordre que les étiquettes) et un tableau des paramètres et de leur valeur en fonction des différentes injections (pour les paramètres qui varient selon le type d'injection).
 Et enfin la fonction définissant le champ de vecteur.
 
 Tous ces arguments sont créés en dehors du serve et de l'ui
 
 
 Pour la partie UI du module, les arguments sont le nom de l'image décrivant le modèle (si pas d'image mettre un nom au hasard, R le cherchera dans le fichier "www" et ne le trouvera pas et pas grave), et les dimensions d'affichage de cette image
 
 ## Ajouter un modèle
 
 On garde la structure de l'UI et on ajoute dans un tabPanel le module
 
 Pour la partie Server :
 Toujours garder les 4 premières lignes et ajouter (peut importe la place tant que c'est au sein du server) un objet dans lequel on va stocker le résultat du moduleServer avec en arguments tous les objets déja créés ainsi qu'un nom au module (repris dans la partie ui, pour que R mette en lien les parties UI et Server du module), le nom du modèle.

## Mise en place comparaison :

Dans l'exemple déjà crée ici, le module de comparaison est déja implémenté au niveau de l'UI,
Lors de l'ajout d'un modèle, il faut mettre à jour la partie server pour prendre en compte cet ajout :
dans le observe: ajouter à chaque objet (MC, brut etc..) le nom de l'objet dans lequel on stocke le module qui a ajouté le nouveau modèle dans la liste.
dans le module server de comparaison, ajouter juste le nom de ce nouveau modèle dans la liste des noms ( doit ^tre identique au nom donné en argument au module qui implémente ce modèle
