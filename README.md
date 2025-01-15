Ce qui a été réalisé :

- Toutes les tâches de base ont été réalisé et testé, arithmétique, variables, classes et attributs, méthodes et héritages.

- Les extensions suivantes on été rajoutées (Avec plus de détails sur l'implémentaion dans les sections suivantes) :
- Les tableaux : On a tous ce qu'apporte un langage de programmation bas niveau avec des tableaux primitifs, c'est à dire une taille fixe, pas de fonction length
- attributs immuables : ajout du mot clé final
- déclaration en série.
- initialisation à la déclaration.
- Égalité structurelle
- Accesibilité : Protected, Private

1) Analayse Syntaxique (Parser & Lexer) : 

- une des difficulté dans la partie syntaxique, c'était le conflit des opération binaires, alors qu'il me semblait que toutes les priorités ont été ajusté, j'avais toujours des conflits et je ne comprenais pas du tout d'ou ça pouvait venir, finalement j'ai trouvé la directive qui m'a sauvé dans le cours qui est %inline.

- Tableaux : 
    - les choix syntaxique sont, un tableau est déclaré avec par exemple : var int [] tab;
    - deux choix pour initialisé le tableaux, soit avec : tab = new[] int [n], avec n la taille du tableau, pour éviter des conflits reduce/reduce le choix de syntaxe new[] a été fait au lieu de new tout seul.
    - deuxème méthodes c'est avec une liste d'élement : tab = [1,2,3];
    - pour accèder à un élément du tableau : tab.[indice], pareil pour un problème de conflit le choix tab.[] au lieu de tab[]
    - A noter que pour l'intialisation avec new[], la manière la plus agréable pour moi pour gérer les tableau à plusieur dimensions c'était de séparer les types en types primitif (TInt,..,TClasse) et le type des tableaux, donc pour récuperer le type récursif ( par TArray(Tarray(Tarray int)) pour new[] int [n1][n2][n3]) il ya une fonction dans le typechekcer

- pour final, déclaration en série, init à la déclaration rien de spécial niveau syntaxique 

2) Vérification de types : 

- En plus de ce qui a été demandé dans l'énoncé je vérifie qu'il n'ya pas deux variables, attributs, classes, sur le même niveau de visibilité avec le même identifiant.

- J'impose que le constructeur doit être la premiere méthode d'une classe si o veut en mettre un

- Au niveau de la syntaxe abstraite, pour identifier un attribut final et pour passé une valeur à la déclaration on a respectivement un booleen et une reference vers un type option d'une expression pour les attributs de classes, pour les variables on a que le champs option pour une eventuelle intialisation à la déclaration.

- On vérifie bien sur le type de l'expression avec laquelle on initialise avec le type statique

- Tableaux : 

- J'ai rajouté le type TArray of typ
- Je laisse une petite souplesse entre new et [] c'est que un tableau init avec new peut etre rechanger de forme
de taille etc

-les règles d'inférence suivante ont été utilisées :

  t appartient [int,TClass,bool]      E |- ek : tk'      tk' <: t
  -------------------------------------------------------------------
                E |- NEW[] t [e1][e2][e3]...[ek]: t[][]..[]


    E |- e1 : t1        E |- ek : tk'             tk' <: t1
  -------------------------------------------------------------------
                E |-[e1, e2, e3,...,ek]: t[]


    E |- e1 : t[]         E |- e2 : int             
  -------------------------------------------------------------------
                    E |- e1.[e2]: t

- On a aussi une erreur levée pour un tableau de void
- | VArray of value array 

- final : dans checkclass, on vérifie bien que tout attributs final non init à la déclaration l'est dans le constructeur, on leve une erreur sinon.

    - après pour vérifier qu'on modifie pas un final il suffit de vérifier dans Set(mem,e)
    que si mem est un accès à un attribut final alors init est à None sinon on leve une erreur. (vu qu'on a vérifier avant qu'il ya un constructeur pour initialiser et que le constructeur est la premiere méthode dans une classe alors pas de risque que l'initialisation se fasse dans une autre méthode)

3) interpreteur : 
Leve les erreurs attendus à l'execution, division par 0, acces indice invalide du tableau.
   
- Extensions :
  - initialisation à la déclaration : pour l'implementer je rajoute de façon implicite des instruction Set() au tout début de la fonction main comme ça l'execution se fera avec l'environement des variable globales 
  - égualité structurelle : implémenté par la fonction récursive eq_structurelle, pour un objet ou un tableau vérifie que tous les attributs (ou cases de tableau) ont les mêmes valeurs 


Problème : 
- met List.exist à la place de List.forrall !!!
