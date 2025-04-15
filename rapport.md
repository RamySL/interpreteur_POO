# Rapport de Projet : Interpréteur KAWA

**Auteur : Ramy SAIL LDD3 MAG** 

## Ce qui a été réalisé

- Toutes les tâches de base ont été réalisées et testées : arithmétique, variables, classes et attributs, méthodes et héritages.
- Extensions ajoutées :
  - **Tableaux** : Tableaux primitifs avec taille fixe.
  - **Attributs immuables** : ajout du mot-clé `final`.
  - **Déclaration en série**.
  - **Initialisation à la déclaration**.
  - **Égalité structurelle** : `===` et `=/=`.
  - **Accessibilité** : `protected`, `private`.
  - **instanceof**

---

## 1. Analyse Syntaxique (Parser & Lexer)

### Défis rencontrés
- Une des difficultés dans la partie syntaxique était le conflit des opérations binaires. Bien que toutes les priorités aient été ajustées, j'avais toujours des conflits et je ne comprenais pas d'où cela pouvait venir. Finalement, j'ai trouvé la directive `%inline` dans le cours, qui m'a sauvé.

### Tableaux
- Déclaration : `var type [] tab;`
- Initialisation : Deux façons de faire :
  - `tab = new[] int [n]` avec `n` comme taille. Pour éviter des conflits reduce/reduce, le choix de syntaxe `new[]` a été fait au lieu de `new` tout seul.
  - `tab = [1,2,3];`

- Accès : `tab.[indice]` (choix syntaxique pour éviter des conflits avec `tab[]`).
- À noter que pour l'initialisation avec `new[]`, la manière la plus agréable pour moi de gérer les tableaux à plusieurs dimensions était de séparer les types en types primitifs (`TInt`, ..., `TClasse`) et le type des tableaux. Pour récupérer le type récursif (par exemple `TArray(TArray(TArray int))` pour `new[] int [n1][n2][n3]`), il y a la fonction `get_recTyp_from_ArrayDecl` dans le `typechecker`.

### Déclaration en série
- Comme dans tous les langages : `type a, b, c;`
- J'utilise un non-terminal qui récupère la liste des variables déclarées en série. Après, le type déclaré au début est appliqué à toutes les variables.

### Accessibilité
- Par défaut, si non spécifiée, elle est mise à `protected`.

---

## 2. Vérification de Types

### Règles générales
- Vérification d'unicité des identifiants au même niveau de visibilité (variables, attributs, classes).
- Le constructeur doit être la première méthode d'une classe.

### Tableaux
- Introduction du type `TArray of typ`.
- Vérification si un tableau a le type `void` pour lever une erreur.
- Vérification que l'accès au tableau est correct, par exemple accès à une dimension inexistante.
- Tous les cas de tests sont dans `./test/tableaux.kwa`

#### Règles d'inférence utilisées pour les tableaux :
```
t ∈ [int, TClass, bool]      E |- ek : tk'      tk' <: t
---------------------------------------------------------
      E |- NEW[] t [e1][e2][e3]...[ek]: t[][]..[]

E |- e1 : t1        E |- ek : tk'             tk' <: t1
---------------------------------------------------------
                E |-[e1, e2, ..., ek]: t[]

          E |- e1 : t[]         E |- e2 : int
---------------------------------------------------------
                    E |- e1.[e2]: t
```

### `final`

- Un attribut `final` non initialisé à la déclaration doit obligatoirement être initialisé dans le constructeur.
- Le type de l'expression d'initialisation doit correspondre au type déclaré statiquement.
- Au niveau de la syntaxe abstraite, pour identifier un attribut `final`, on a un booléen.
- Dans `checkclass`, on vérifie bien que tout attribut `final` non initialisé à la déclaration l'est dans le constructeur. On lève une erreur sinon.

    - Après, pour vérifier qu'on ne modifie pas un `final`, il suffit de vérifier dans `Set(mem, e)` que si `mem` est un accès à un attribut `final`, alors `init` est à `None`. Sinon, on lève une erreur (puisqu'on a vérifié avant qu'il y a un constructeur pour initialiser et que le constructeur est la première méthode dans une classe, il n'y a donc pas de risque que l'initialisation se fasse dans une autre méthode).

### Initialisation à la déclaration

- Pour les attributs, j'utilise une référence vers un type `option` d'expression au niveau de la syntaxe abstraite. La référence n'a pas vraiment de relation avec l'implémentation de l'initialisation à la déclaration, mais plutôt pour traiter les attributs `final`. Un attribut `final` non initialisé à la déclaration change la valeur de la référence lorsqu'il y a une affectation à l'attribut.
- Pour les variables, pas besoin de référence et de traitement spécifique, donc un type `option expr` est utilisé.

### Égalité structurelle

```
  E |- e1 : t1    E |- e2 : t2      t1=t2
-----------------------------------------------
          E |- e1 === e2: bool

  E |- e1 : t1    E |- e2 : t2      t1=t2
-----------------------------------------------
          E |- e1 =/= e2: bool
```

### Accessibilité

- J'ai rajouté le type algébrique `accessibility` dans la syntaxe abstraite.
- Les méthodes qui remontent la hiérarchie de classes pour chercher une méthode ou un attribut recherchent uniquement parmi ceux qui ont l'accès `protected` dès qu'on est en train de rechercher dans une superclasse.

### `instanceof`

- Fait partie des expressions.
- À ce niveau, on vérifie que si le type statique est un sous-type, si ce n'est pas le cas, on change la référence vers le booléen à `false`.

```
    t = TClass      E |- ek : TClass
------------------------------------------
        E |- e instanceof t: bool
```

---

## 3. Interpréteur

### Gestion des erreurs
- Levée des erreurs attendues à l'exécution :
  - Division par zéro.
  - Accès à un indice invalide dans un tableau.

### Tableaux
- Les tableaux sont implémentés par `Array`. À l'initialisation, le tableau est rempli par `Null`. C'est fait par la fonction `create_array`.

### `final`
- Pas de traitement au niveau de l'interprétation, tout a été réalisé par le `typechecker`.

### Initialisation à la déclaration

- Pour l'implémenter, j'ajoute de façon implicite des instructions `Set()` au tout début de la fonction `main` pour chaque initialisation, de sorte que l'exécution se fasse avec l'environnement des variables globales.

### Égalité structurelle

- Implémentée par la fonction récursive `eq_structurelle`, qui vérifie, pour un objet ou un tableau, que tous les attributs (ou cases de tableau) ont les mêmes valeurs.

### Accessibilité
- Pas de traitement au niveau de l'interprétation, tout a été réalisé par le `typechecker`.

### instanceof

- Utilise la référence vers le booléen. Si l'objet que l'on veut vérifier n'est pas instancié, sinon remonte la hiérarchie de classes pour chercher le deuxième opérande de `instanceof`.

---
