Set Warnings "-notation-overridden,-parsing".

From Coq Require Import Bool.Bool.
From Coq Require Import Init.Nat.
From Coq Require Import Arith.Arith.
From Coq Require Import Arith.EqNat.
From Coq Require Import omega.Omega.
From Coq Require Import Lists.List.
From Coq Require Import Strings.String.
Import ListNotations.

Module TM.

Inductive letter : Type :=
  | RightBounding
  | LeftBounding
  | Empty
  | Letter {A} (a : A).

Definition alphabet {A : Type} : Type := list A.

Definition states {S : Type} : Type := list S.

Inductive triple {S : Type} : Type := 
  | Triple (l : letter) (s : S) (r : letter).

Notation "( l , s , r )" := (triple l s r).

Inductive command {S : Type} : Type :=
  | Command (t1 t2 : @triple S).

Notation "t1 -> t2" := (command t1 t2).

Check @Letter nat 1.
Check Letter 1.
Check triple Empty 2 Empty.
Check (triple (Letter 2) 2 (Letter 3)).
Check (Empty, 2, Empty).
Check ((Letter 1), 2, (Letter 3)).
Check (Letter 1, 2, Letter 3) -> (Letter 2, 2, Letter 5).

Inductive TM {A S} : Type :=
  | tm  (inp : @alphabet A)
        (tp : list (@alphabet A))
        (tpst : list (@states S))
        (cmds : list (list (@command A S)))
        (st : @states S)
        (fn : @states S).

Inductive config {A S} : Type :=
  | Config (l : list A) (s : S) (r : list A).

Definition tm_configuration {A S} := list (@config A S).

Fixpoint make_config {A S}
    (left : list (list A)) (statess : list S) (right : list (list A)) : tm_configuration :=
      match left, statess, right with
      | [], [], [] => []
      | lh :: lt, sh :: st, rh :: rt => Config lh sh rh :: make_config lt st rt
      | [], sh :: st, [] => Config [] sh [] :: make_config [] st []
      | lh :: lt, sh :: st, [] => Config lh sh [] :: make_config lt st []
      | [], sh :: st, rh :: rt => Config [] sh rh :: make_config [] st rt
      | _, _, _ => []
      end.

Example e4 : make_config [[1]] [2] [[3]] = [Config [1] 2 [3]].
Proof. reflexivity. Qed.

Example e5 : make_config [[1]; [2]] [3] [[4]] = [Config [1] 3 [4]].
Proof. reflexivity. Qed.

Example e6 : make_config [[1]; [2]] [3; 4] [[5]] = [Config [1] 3 [5]; Config [2] 4 []].
Proof. reflexivity. Qed.

Definition start_config {A S} (input : list A) (st : @states S) : tm_configuration :=
  make_config [input] st [].

Fixpoint is_access {A S} (cfgs : @tm_configuration A S) (ast : @states S) :=
  match cfgs, ast with
  | [], [] => True
  | (Config l s r) :: tc, ha :: ta => s = ha /\ is_access tc ta
  | _, _ => False
  end.

(*
s1 == s && 
(r1 == emptySymbol && l1 == rightBoundingLetter && r2 /= emptySymbol && l2 == rightBoundingLetter && l == [rightBoundingLetter] ||
r /= [leftBoundingLetter] && r1 == (last r) && l1 == rightBoundingLetter && r2 == emptySymbol && l2 == rightBoundingLetter && l == [rightBoundingLetter] ||
(last r) == r1 && (head l) == l1 ||
r1 == emptySymbol && r2 == emptySymbol && l1 == rightBoundingLetter && l2 == l1) -> checkCommandTapeToTape t2 t1
*)

Fixpoint get_applicable_cmds {A S} (cfgs : @tm_configuration A S) (cmds : list (list (@command A S))) 
  : list (@command A S) :=
    let check cmd :=
      match cfgs, cmd with
      | [], [] => []
      | (Config l1 s1 r1) :: tcfg, (Command l2 s2 r2) :: tcmd => s1 = s2 /\ 

Fixpoint interpret_tm {A} (tm : TM) (inp : list A) : bool :=
  match tm with 
  | ia ta ts cmds stst ast => 
    let interpret input cfgs :=
      match input with
      | [] => is_access cfgs ast
      | h :: t => 

    in
    interpret inp (start_config inp stst)






