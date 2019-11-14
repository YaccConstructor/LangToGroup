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

Definition command_right_form {S} (A : Type) (cmd : @command S) := 
  match cmd with
  | (l1, s1, r1) -> (l2, s2, r2) =>
      r1 <> LeftBounding /\ r2 <> LeftBounding /\ l1 <> RightBounding /\ l2 <> RightBounding /\
      (r1 = r2 /\ (l1 = l2 \/ r1 = RightBounding /\ l1 <> LeftBounding /\ l2 <> LeftBounding) \/
      (exists (a1 a2 a3 a4 : A), l1 = L/a1 /\ l2 = L/a2 /\ r1 = L/a3 /\ r2 = L/a4))
  end.

Definition command_applicable {S} (A : Type) (cmd : @command S) (cfg : @config S) :=
  match cmd, cfg with
  | (l1, s1, r1) -> (l2, s2, r2), Config l s r => 
      s1 = s /\ 
      (l1 = Empty /\ r1 = RightBounding /\ r = [r1] \/
      r1 = RightBounding /\ r = [r1] /\ l2 = Empty /\ l1 = (last l LeftBounding) \/
      (exists (a1 a2 : A), l1 = L/a1 /\ r1 = L/a2 /\ (last l LeftBounding) = l1 /\ (hd RightBounding r) = r1))
  end.

Compute last [] 5.
Compute hd 3 [].

Definition apply_command {S} (cmd : @command S) (cfg : @config S) :=
  match cmd, cfg with
  | (l1, s1, r1) -> (l2, s2, r2), Config l s r => 
    match l1, l2 with
    | Empty, Empty => Config l s2 r
    | Empty, _ => Config (l ++ [l2]) s2 r
    | _, Empty => Config (removelast l) s2 r
    | _, _ => Config ((removelast l) ++ [l2]) s2 (r2 :: (tl r))
    end
  end.

Lemma next_cfg_exists : forall (A S : Type) (cmd : @command S) (cfg : @config S),
  command_right_form A cmd /\ command_applicable A cmd cfg ->
  (exists new_cfg, apply_command cmd cfg = new_cfg).
Proof.
  intros A S cmd cfg [H1 H2].
  destruct cmd. destruct t1. destruct t2. destruct cfg.
  unfold command_right_form in H1.
  unfold command_applicable in H2.
  unfold apply_command.
  destruct H1 as [H0 [H1 [H3 [H4 [[H5 [H6|H6]]|H5]]]]].
  - destruct H2 as [H2 [[H7 [H8 H9]]|[[H7 [H8 [H9 H10]]]|[a1 [a2 [H7 [H8 [H9 H10]]]]]]]].
    + subst. exists (Config l1 s0 [RightBounding]). reflexivity.
    + subst. exists (Config l1 s0 [RightBounding]). reflexivity.
    + subst. exists (Config (removelast l1 ++ [Letter a1]) s0 (Letter a2 :: tl r1)). reflexivity.
  - destruct H2 as [H2 [[H7 [H8 H9]]|[[H7 [H8 [H9 H10]]]|[a1 [a2 [H7 [H8 [H9 H10]]]]]]]].
    + subst. destruct l0.
      * destruct H4. reflexivity.
      * destruct H6 as [_ [_ []]]. reflexivity.
      * exists (Config l1 s0 [RightBounding]). reflexivity.
      * exists (Config (l1 ++ [Letter a]) s0 [RightBounding]). reflexivity.
    + subst. destruct (last l1 LeftBounding).
      * simpl. destruct H3. reflexivity.
      * destruct H6 as [_ [H6]]. destruct H6. reflexivity.
      * exists (Config l1 s0 [RightBounding]). reflexivity.
      * exists (Config (removelast l1) s0 [RightBounding]). reflexivity.
    + subst. destruct l0.
      * destruct H4. reflexivity.
      * destruct H6 as [_ [_ []]]. reflexivity.
      * exists (Config (removelast l1) s0 r1). reflexivity.
      * destruct H6 as [H6 _]. discriminate H6.
  - destruct H2 as [H2 [[H7 [H8 H9]]|[[H7 [H8 [H9 H10]]]|[a5 [a6 [H7 [H8 [H9 H10]]]]]]]]; 
    destruct H5 as [a1 [a2 [a3 [a4 [H5 [H11 [H12 H13]]]]]]]; subst; try discriminate.
    + exists (Config (removelast l1 ++ [Letter a2]) s0 (Letter a4 :: tl r1)). reflexivity.
  Qed.

(*
((l1, s1, r1) -> (l2, s2, r2)) : t1, (l, s, r) : t2) => s1 = s /\ 
  (l1 = Empty /\ r1 = RightBounding /\ r = [r1] \/
  l <> [LeftBounding] /\ l1 = (last l) /\ r1 = RightBounding /\ l2 = Empty /\ r2 = r1 /\ r = [r1] \/
  (last l) = l1 /\ (head r) = r1) /\ checkCommandTapeToTape t2 t1
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






