# Interpréteur KAWA

Le rapport détaillé des fonctionnalités implémentées pour le langage est disponible dans le fichier [rapport.md](rapport.md).

### Description des fichiers

- **kawa.ml** : Détermine les expressions, instructions du programme, ainsi que la structure et les types des classes et des méthodes.
- **kawai.ml** : Point d'entrée, il prend un programme source `.kawa` et l'exécute.

Voici la description des fichiers donnée dans l'ordre de l'interprétation d'un langage de programmation :

1. **kawalexer.mll** : Fichier de l'analyseur lexical (en OCamllex).
2. **kawaparser.mly** : Fichier de l'analyse syntaxique (avec Menhir).
3. **typechecker.ml** : Vérificateur de types.
4. **interpreter.ml** : Fichier de l'interpréteur du code qui exécute le programme.

### Dossier tests

Le dossier **tests** contient des fichiers source en langage KAWA pour tester l'interpréteur.
