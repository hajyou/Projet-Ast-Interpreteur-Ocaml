**OCaml • AST • Interpréteur**

Implémentation d’un mini langage impératif en OCaml basé sur un arbre de syntaxe abstraite (AST).

Le projet modélise :

- Des expressions arithmétiques
- Des expressions logiques
- Des instructions impératives (Define, Sequence, Repeat)

---

## 📚 Structure du langage

### Opérations arithmétiques

- Plus
- Moins
- Fois
- Div

### Comparaisons

- Eq
- Sup
- Inf

### Instructions

- `Define (id, expr)`
- `Sequence (m1, m2)`
- `Repeat (instruction, condition)`

---

## 🧱 Exemple de programme construit

```ocaml
let x = Define ("x", Cst 5);;
let y = Define ("y", Cst 37);;
let z = Define ("z", Opbin (Plus, Var "x", Var "y"));;
let seq = Sequence (x, Sequence (y, z));;

print_endline (afficher_exp_bcl seq);;
```

Sortie :

```
x <- 5 ; y <- 37 ; z <- (x + y)
```

---

## ⚙️ Technologies utilisées

- OCaml
- Programmation fonctionnelle
- Modélisation d’AST
- Pattern matching
- Pretty-printing
- Interprétation partielle d’expressions

---

## 🚀 Compilation

### Compilation simple

```bash
ocamlc Ast_Interpreter.ml
```
---

## 📌 Fonctionnalités

- Modélisation complète d’un AST
- Pretty-printer des instructions
- Interpréteur partiel des expressions logiques
- Structuration modulaire des types

---

## 🧠 Finalité du projet

Ce projet illustre :

- La conception d’un langage miniature
- La manipulation d’arbres de syntaxe abstraite
- L’utilisation avancée du pattern matching
- Les bases de la construction d’un interpréteur

---

## 👤 Auteur

HAJRI Youssef  
Étudiant en Informatique
