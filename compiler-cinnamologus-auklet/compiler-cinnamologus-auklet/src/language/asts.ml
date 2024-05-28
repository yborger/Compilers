(* Contains the AST definitions for the Auklet language. *)

(** The unary operators in the language. *)
type unary_operator =
  | OpAfter
  | OpBefore
[@@deriving eq, ord, show]
;;

(** The binary oeprators in the language. *)
type binary_operator =
  | OpPlus
  | OpMinus
  | OpTimes
[@@deriving eq, ord, show]
;;

(** Expressions in the language. *)
type expr =
  | EInt of int
  | EVar of string
  | EUnaryOp of unary_operator * expr
  | EBinaryOp of binary_operator * expr * expr
  | ELet of string * expr * expr
[@@deriving eq, ord, show]
;;
