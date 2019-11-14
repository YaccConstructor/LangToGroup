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

Notation "( l , s , r )" := (Triple l s r).

Inductive command {S : Type} : Type :=
  | Command (t1 t2 : @triple S).

Notation "t1 -> t2" := (Command t1 t2).

Notation "L / x" := (Letter x).

Check @Letter nat 1.
Check Letter 1.
Check (Triple (Letter 2) 2 (Letter 3)).
Check (Empty, 2, Empty).
Check ((Letter 1), 2, (Letter 3)).
Check (Letter 1, 2, Letter 3) -> (Letter 2, 2, Letter 5).
Check (L/1, 2, L/3) -> (L/2, 2, L/5).

Inductive TM {A S} : Type :=
  | tm  (inp : @alphabet A)
        (tp : list (@alphabet A))
        (tpst : list (@states S))
        (cmds : list (list (@command S)))
        (st : @states S)
        (fn : @states S).

Inductive config {S} : Type :=
  | Config (l : list letter) (s : S) (r : list letter).

Definition tm_configuration {S} := list (@config S).

Search fold_left.
Compute fold_left (fun x y => x ++ y) [[0]; [3]] [5]. (** [5; 0; 3] **)

Fixpoint make_start_config {S A}
    (inp : list A) (sts : list S) : tm_configuration :=
    match sts with
    | [] => []
    | sh :: st =>
      let letter_inp := (LeftBounding :: map (fun i => L/i) inp) ++ [RightBounding] in
      (Config letter_inp sh [RightBounding]) :: map (fun s => Config [LeftBounding] s [RightBounding]) st
    end.

Example e4 : make_start_config [1; 2; 3] [0] 
  = [Config [LeftBounding; L/1; L/2; L/3; RightBounding] 0 [RightBounding]].
Proof. reflexivity. Qed.

Fixpoint is_access {S} (cfgs : @tm_configuration S) (ast : @states S) :=
  match cfgs, ast with
  | [], [] => True
  | (Config l s r) :: tc, ha :: ta => s = ha /\ is_access tc ta
  | _, _ => False
  end.

(*
s1 == s && 
(r1 == emptySymbol && l1 == rightBoundingLetter && l2 == l1 && l == [] ||
r1 == (last r emptySymbol) && l1 == rightBoundingLetter && r2 == emptySymbol && l2 == l1 && l == [] ||
(last r) == r1 && (hd emptySymbol l) == l1 && r2 <> leftBoundingLetter && l2 <> rightBoundingLetter) -> checkCommandTapeToTape t2 t1
*)

Fixpoint get_applicable_cmds {A S} (cfgs : @tm_configuration A S) (cmds : list (list (@command A S))) 
  : list (@command A S) :=
    let check cmd :=
      match cfgs, cmd with
      | [], [] => []
      | (Config l s r) :: tcfg, ((l1, s1, r1) -> (l2, s2, r2)) :: tcmd => s1 = s /\ (l1 = )

Fixpoint interpret_tm {A} (tm : TM) (inp : list A) : bool :=
  match tm with 
  | ia ta ts cmds stst ast => 
    let interpret input cfgs :=
      match input with
      | [] => is_access cfgs ast
      | h :: t => 

    in
    interpret inp (start_config inp stst)






