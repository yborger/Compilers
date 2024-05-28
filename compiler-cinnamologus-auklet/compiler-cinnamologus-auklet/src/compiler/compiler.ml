(* This file contains the definition of the compiler: the tool that translates
    an AST in our language into assembly language. *)

open Batteries;;
open HatchLanguage;;

open Assembly;;
open Asts;;
open Environment;;
open Printf;;

let rec compile_expression (env : environment) (e : expr)
  : instruction list =
    match e with
  | EInt(int_input)-> [(AsmMov(ArgRegister(RAX) ,ArgConstant(string_of_int(int_input))))]
  | EVar(string_input) -> [(AsmMov(ArgRegister(RAX), find_named_variable string_input env))]
  | EUnaryOp(un_op, expr_input) -> 
      begin 
      match un_op with 
      | OpAfter -> compile_expression env expr_input @[AsmAdd(ArgRegister(RAX), ArgConstant("1"))]
      | OpBefore -> compile_expression env expr_input @[AsmSub(ArgRegister(RAX), ArgConstant("1"))]
      end 
  | EBinaryOp(bin_op, expr_input1, expr_input2) -> 
    let (arg_expr1, new_env) = allocate_temp_variable env in 
    let expr1_eval = compile_expression new_env expr_input1 in 
    let expr1_instructions = [(AsmMov(arg_expr1, ArgRegister(RAX)))] in 
    let (arg_expr2, new_env2) = allocate_temp_variable new_env in 
    let expr2_eval = compile_expression new_env2 expr_input2 in 
      begin
      match bin_op with
      | OpPlus -> 
        let expr2_instructions = [(AsmMov(arg_expr2, ArgRegister(RAX)))] in 
        let add_instructions = [AsmAdd(ArgRegister(RAX), arg_expr1)] in 
        expr1_eval @ expr1_instructions @ expr2_eval @ expr2_instructions @ add_instructions 
      | OpMinus -> 
        let expr2_instructions = [(AsmMov(arg_expr2, ArgRegister(RAX)))] in 
        let minus_instructions = [AsmSub(arg_expr1, ArgRegister(RAX))] in 
        let store_result_rax = [(AsmMov(ArgRegister(RAX), arg_expr1))] in 
        expr1_eval @ expr1_instructions @ expr2_eval @ expr2_instructions @ minus_instructions @ store_result_rax
      | OpTimes -> 
        let expr2_instructions = [(AsmMov(arg_expr2, ArgRegister(RAX)))] in 
        let times_instructions = [AsmIMul(ArgRegister(RAX),arg_expr1)] in 
        expr1_eval @ expr1_instructions @ expr2_eval @ expr2_instructions @times_instructions 
      end
  | ELet(var_name,expr_input1,expr_input2) -> 
      let new_env1 = allocate_named_variable var_name env in 
      let expr1_instructions = compile_expression new_env1 expr_input1 in 
      let stack_address_of_var = find_named_variable var_name new_env1 in 
      let store_instruction = [(AsmMov(stack_address_of_var,ArgRegister(RAX)))] in 
      let expr2_instruction = compile_expression new_env1 expr_input2 in 
      expr1_instructions @ store_instruction @ expr2_instruction   
;;

let compile_program (e : expr) : instruction list =
  let instructions = compile_expression empty_environment e in
  instructions @ [AsmRet]
;;

let compile_to_assembly_code (e : expr) : string =
  let instructions = compile_program e in
  let instruction_code = code_of_instruction_list instructions in
  "section .text\n" ^
  "global bird_main\n" ^
  "bird_main:\n" ^
  instruction_code ^
  "\n"
;;
