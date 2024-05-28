(* This file contains type declarations and functions related to the compiler's
    representation of x86-64 assembly language. *)

open Batteries;;

(** Describes the registers of x86-64 that our code will use. *)
type register =
  | RAX
  | RSP
;;

(** Describes a memory address expression in x86-64 assembly. *)
type address =
  | AddrByRegister of register
  | AddrByRegisterOffset of register * int
;;

(** Describes the type of arguments in our x86-64 assembly representation.  We
    use this type somewhat loosely: not every argument is valid everywhere an
    argument type is written below, but capturing the precise syntax limitations
    of x86 would make our assembly language types a lot more complicated.

    Note that the string argument of ArgConstant is the textual representation
    of the constant to be emitted to the assembly file, such as "5" or
    "0xFFFFFFFFFFFFFFFE".
*)
type argument =
  | ArgConstant of string
  | ArgRegister of register
  | ArgMemory of address
;;

(** The type that represents single x86 instructions. *)
type instruction =
  | AsmAdd of argument * argument
  | AsmIMul of argument * argument
  | AsmMov of argument * argument
  | AsmSub of argument * argument
  | AsmRet
;;

(** A function which transforms an x86 register into a string suitable for
    writing into an assembly language file. *)
let code_of_register (register : register) : string =
  match register with 
  | RAX -> "rax"
  | RSP -> "rsp"
;;

(** A function which transforms an x86 address expression into a string suitable
    for writing into an assembly language file. *)
let code_of_address (address : address) : string =
  match address with
  | AddrByRegister (register) -> "[" ^code_of_register register ^"]"
  | AddrByRegisterOffset (register, int) -> "[" ^ code_of_register register ^ string_of_int(int) ^ "]" (*i.e. esi-4*)
;;

(** A function which transforms an x86 argument into a string suitable for
    writing into an assembly language file. *)
let code_of_argument (argument : argument) : string =
  match argument with 
  | ArgConstant(string_input) -> string_input
  | ArgRegister(register) -> code_of_register(register)
  | ArgMemory(address) -> code_of_address(address)
;;

(** A function which transforms an x86 instruction into a string suitable for
    writing into an assembly language file.  For example, given the input
    AsmRet, an appropriate return value might be "  ret\n".
  *)
let code_of_instruction (instruction : instruction) : string =
  match instruction with 
  | AsmAdd(argument1, argument2) -> "add" ^ " " ^ (code_of_argument(argument1)) ^ "," ^ (code_of_argument(argument2))
  | AsmIMul(argument1, argument2) -> "imul" ^ " " ^ (code_of_argument(argument1)) ^ "," ^ (code_of_argument(argument2))
  | AsmMov(argument1, argument2) -> "mov" ^ " " ^ (code_of_argument(argument1)) ^ "," ^ (code_of_argument(argument2))
  | AsmSub(argument1, argument2) -> "sub" ^ " " ^ (code_of_argument(argument1)) ^ "," ^ (code_of_argument(argument2))
  | AsmRet -> "ret"
;;

(** A function which transforms a list of x86 instructions into a string
    suitable for writing into an assembly language file. *)
let rec code_of_instruction_list (instruction_list : instruction list) : string =
  match instruction_list with 
  | [] -> "" 
  | element :: rest -> code_of_instruction(element) ^ "\n" ^ code_of_instruction_list(rest)
;;
