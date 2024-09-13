(**
   Boilerplate to be used as a template when mapping the move_on_sui CST
   to another type of tree.
*)

module R = Tree_sitter_run.Raw_tree

(* Disable warnings against unused variables *)
[@@@warning "-26-27"]

(* Disable warning against unused 'rec' *)
[@@@warning "-39"]

type env = unit

let token (env : env) (tok : Tree_sitter_run.Token.t) =
  R.Token tok

let blank (env : env) () =
  R.Tuple []

let map_modifier (env : env) (x : CST.modifier) =
  (match x with
  | `Public tok -> R.Case ("Public",
      (* "public" *) token env tok
    )
  | `Publ_83d19bc tok -> R.Case ("Publ_83d19bc",
      (* "public(package)" *) token env tok
    )
  | `Publ_7c2e49a tok -> R.Case ("Publ_7c2e49a",
      (* "public(friend)" *) token env tok
    )
  | `Entry tok -> R.Case ("Entry",
      (* "entry" *) token env tok
    )
  | `Native tok -> R.Case ("Native",
      (* "native" *) token env tok
    )
  )

let map_pat_2f1c977 (env : env) (tok : CST.pat_2f1c977) =
  (* pattern 0x[a-fA-F0-9_]+ *) token env tok

let map_bool_literal (env : env) (x : CST.bool_literal) =
  (match x with
  | `True tok -> R.Case ("True",
      (* "true" *) token env tok
    )
  | `False tok -> R.Case ("False",
      (* "false" *) token env tok
    )
  )

let map_unary_op (env : env) (x : CST.unary_op) =
  (match x with
  | `BANG tok -> R.Case ("BANG",
      (* "!" *) token env tok
    )
  )

let map_spec_apply_name_pattern (env : env) (tok : CST.spec_apply_name_pattern) =
  (* pattern [0-9a-zA-Z_*]+ *) token env tok

let map_address_literal (env : env) (tok : CST.address_literal) =
  (* pattern @0x[a-fA-F0-9]+ *) token env tok

let map_byte_string_literal (env : env) (tok : CST.byte_string_literal) =
  (* pattern "b\"(\\\\.|[^\\\\\"])*\"" *) token env tok

let map_spec_condition_kind (env : env) (x : CST.spec_condition_kind) =
  (match x with
  | `Assert tok -> R.Case ("Assert",
      (* "assert" *) token env tok
    )
  | `Assume tok -> R.Case ("Assume",
      (* "assume" *) token env tok
    )
  | `Decres tok -> R.Case ("Decres",
      (* "decreases" *) token env tok
    )
  | `Ensures tok -> R.Case ("Ensures",
      (* "ensures" *) token env tok
    )
  | `Succes_if tok -> R.Case ("Succes_if",
      (* "succeeds_if" *) token env tok
    )
  )

let map_pat_3937285 (env : env) (tok : CST.pat_3937285) =
  (* pattern [0-9][0-9_]*(?:u8|u16|u32|u64|u128|u256)? *) token env tok

let map_identifier (env : env) (tok : CST.identifier) =
  (* pattern (`)?[a-zA-Z_][0-9a-zA-Z_]*(`)? *) token env tok

let map_anon_choice_AMP_c6caa5d (env : env) (x : CST.anon_choice_AMP_c6caa5d) =
  (match x with
  | `AMP tok -> R.Case ("AMP",
      (* "&" *) token env tok
    )
  | `AMPmut tok -> R.Case ("AMPmut",
      (* "&mut" *) token env tok
    )
  )

let map_ability (env : env) (x : CST.ability) =
  (match x with
  | `Copy tok -> R.Case ("Copy",
      (* "copy" *) token env tok
    )
  | `Drop tok -> R.Case ("Drop",
      (* "drop" *) token env tok
    )
  | `Store tok -> R.Case ("Store",
      (* "store" *) token env tok
    )
  | `Key tok -> R.Case ("Key",
      (* "key" *) token env tok
    )
  )

let map_hex_string_literal (env : env) (tok : CST.hex_string_literal) =
  (* pattern "x\"[0-9a-fA-F]*\"" *) token env tok

let map_primitive_type (env : env) (x : CST.primitive_type) =
  (match x with
  | `U8 tok -> R.Case ("U8",
      (* "u8" *) token env tok
    )
  | `U16 tok -> R.Case ("U16",
      (* "u16" *) token env tok
    )
  | `U32 tok -> R.Case ("U32",
      (* "u32" *) token env tok
    )
  | `U64 tok -> R.Case ("U64",
      (* "u64" *) token env tok
    )
  | `U128 tok -> R.Case ("U128",
      (* "u128" *) token env tok
    )
  | `U256 tok -> R.Case ("U256",
      (* "u256" *) token env tok
    )
  | `Bool tok -> R.Case ("Bool",
      (* "bool" *) token env tok
    )
  | `Addr tok -> R.Case ("Addr",
      (* "address" *) token env tok
    )
  | `Signer tok -> R.Case ("Signer",
      (* "signer" *) token env tok
    )
  | `Byte tok -> R.Case ("Byte",
      (* "bytearray" *) token env tok
    )
  )

let map_num_literal (env : env) (x : CST.num_literal) =
  (match x with
  | `Pat_3937285 x -> R.Case ("Pat_3937285",
      map_pat_3937285 env x
    )
  | `Pat_2f1c977 x -> R.Case ("Pat_2f1c977",
      map_pat_2f1c977 env x
    )
  )

let map_reserved_identifier (env : env) (x : CST.reserved_identifier) =
  (match x with
  | `Forall tok -> R.Case ("Forall",
      (* "forall" *) token env tok
    )
  | `Exists tok -> R.Case ("Exists",
      (* "exists" *) token env tok
    )
  )

let map_ability_decls (env : env) ((v1, v2, v3) : CST.ability_decls) =
  let v1 = (* "has" *) token env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = map_ability env v1 in
      let v2 = (* "," *) token env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_ability env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3]

let map_postfix_ability_decls (env : env) ((v1, v2, v3, v4) : CST.postfix_ability_decls) =
  let v1 = (* "has" *) token env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = map_ability env v1 in
      let v2 = (* "," *) token env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_ability env x
      ))
    | None -> R.Option None)
  in
  let v4 = (* ";" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

let map_literal_value (env : env) (x : CST.literal_value) =
  (match x with
  | `Addr_lit tok -> R.Case ("Addr_lit",
      (* pattern @0x[a-fA-F0-9]+ *) token env tok
    )
  | `Bool_lit x -> R.Case ("Bool_lit",
      map_bool_literal env x
    )
  | `Num_lit x -> R.Case ("Num_lit",
      map_num_literal env x
    )
  | `Hex_str_lit tok -> R.Case ("Hex_str_lit",
      (* pattern "x\"[0-9a-fA-F]*\"" *) token env tok
    )
  | `Byte_str_lit tok -> R.Case ("Byte_str_lit",
      (* pattern "b\"(\\\\.|[^\\\\\"])*\"" *) token env tok
    )
  )

let rec map_use_member (env : env) (x : CST.use_member) =
  (match x with
  | `Id_COLONCOLON_LCURL_use_member_rep_COMMA_use_member_opt_COMMA_RCURL (v1, v2, v3, v4, v5, v6, v7) -> R.Case ("Id_COLONCOLON_LCURL_use_member_rep_COMMA_use_member_opt_COMMA_RCURL",
      let v1 =
        (* pattern (`)?[a-zA-Z_][0-9a-zA-Z_]*(`)? *) token env v1
      in
      let v2 = (* "::" *) token env v2 in
      let v3 = (* "{" *) token env v3 in
      let v4 = map_use_member env v4 in
      let v5 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = (* "," *) token env v1 in
          let v2 = map_use_member env v2 in
          R.Tuple [v1; v2]
        ) v5)
      in
      let v6 =
        (match v6 with
        | Some tok -> R.Option (Some (
            (* "," *) token env tok
          ))
        | None -> R.Option None)
      in
      let v7 = (* "}" *) token env v7 in
      R.Tuple [v1; v2; v3; v4; v5; v6; v7]
    )
  | `Id_COLONCOLON_id_opt_as_id (v1, v2, v3, v4) -> R.Case ("Id_COLONCOLON_id_opt_as_id",
      let v1 =
        (* pattern (`)?[a-zA-Z_][0-9a-zA-Z_]*(`)? *) token env v1
      in
      let v2 = (* "::" *) token env v2 in
      let v3 =
        (* pattern (`)?[a-zA-Z_][0-9a-zA-Z_]*(`)? *) token env v3
      in
      let v4 =
        (match v4 with
        | Some (v1, v2) -> R.Option (Some (
            let v1 = (* "as" *) token env v1 in
            let v2 =
              (* pattern (`)?[a-zA-Z_][0-9a-zA-Z_]*(`)? *) token env v2
            in
            R.Tuple [v1; v2]
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Id_opt_as_id (v1, v2) -> R.Case ("Id_opt_as_id",
      let v1 =
        (* pattern (`)?[a-zA-Z_][0-9a-zA-Z_]*(`)? *) token env v1
      in
      let v2 =
        (match v2 with
        | Some (v1, v2) -> R.Option (Some (
            let v1 = (* "as" *) token env v1 in
            let v2 =
              (* pattern (`)?[a-zA-Z_][0-9a-zA-Z_]*(`)? *) token env v2
            in
            R.Tuple [v1; v2]
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2]
    )
  )

let map_label (env : env) ((v1, v2) : CST.label) =
  let v1 = (* "'" *) token env v1 in
  let v2 =
    (* pattern (`)?[a-zA-Z_][0-9a-zA-Z_]*(`)? *) token env v2
  in
  R.Tuple [v1; v2]

let map_type_parameter (env : env) ((v1, v2, v3, v4) : CST.type_parameter) =
  let v1 =
    (match v1 with
    | Some tok -> R.Option (Some (
        (* "$" *) token env tok
      ))
    | None -> R.Option None)
  in
  let v2 =
    (match v2 with
    | Some tok -> R.Option (Some (
        (* "phantom" *) token env tok
      ))
    | None -> R.Option None)
  in
  let v3 =
    (* pattern (`)?[a-zA-Z_][0-9a-zA-Z_]*(`)? *) token env v3
  in
  let v4 =
    (match v4 with
    | Some (v1, v2, v3, v4) -> R.Option (Some (
        let v1 = (* ":" *) token env v1 in
        let v2 = map_ability env v2 in
        let v3 =
          R.List (List.map (fun (v1, v2) ->
            let v1 = (* "+" *) token env v1 in
            let v2 = map_ability env v2 in
            R.Tuple [v1; v2]
          ) v3)
        in
        let v4 =
          (match v4 with
          | Some tok -> R.Option (Some (
              (* "+" *) token env tok
            ))
          | None -> R.Option None)
        in
        R.Tuple [v1; v2; v3; v4]
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3; v4]

let map_anon_choice_num_lit_a33e50c (env : env) (x : CST.anon_choice_num_lit_a33e50c) =
  (match x with
  | `Num_lit x -> R.Case ("Num_lit",
      map_num_literal env x
    )
  | `Module_id tok -> R.Case ("Module_id",
      (* pattern (`)?[a-zA-Z_][0-9a-zA-Z_]*(`)? *) token env tok
    )
  )

let map_spec_property (env : env) ((v1, v2) : CST.spec_property) =
  let v1 =
    (* pattern (`)?[a-zA-Z_][0-9a-zA-Z_]*(`)? *) token env v1
  in
  let v2 =
    (match v2 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = (* "=" *) token env v1 in
        let v2 = map_literal_value env v2 in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2]

let map_block_identifier (env : env) ((v1, v2) : CST.block_identifier) =
  let v1 = map_label env v1 in
  let v2 = (* ":" *) token env v2 in
  R.Tuple [v1; v2]

let map_type_parameters (env : env) ((v1, v2, v3, v4, v5) : CST.type_parameters) =
  let v1 = (* "<" *) token env v1 in
  let v2 = map_type_parameter env v2 in
  let v3 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_type_parameter env v2 in
      R.Tuple [v1; v2]
    ) v3)
  in
  let v4 =
    (match v4 with
    | Some tok -> R.Option (Some (
        (* "," *) token env tok
      ))
    | None -> R.Option None)
  in
  let v5 = (* ">" *) token env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

let map_module_identity (env : env) ((v1, v2, v3) : CST.module_identity) =
  let v1 = map_anon_choice_num_lit_a33e50c env v1 in
  let v2 = (* "::" *) token env v2 in
  let v3 =
    (* pattern (`)?[a-zA-Z_][0-9a-zA-Z_]*(`)? *) token env v3
  in
  R.Tuple [v1; v2; v3]

let map_condition_properties (env : env) ((v1, v2, v3, v4) : CST.condition_properties) =
  let v1 = (* "[" *) token env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = map_spec_property env v1 in
      let v2 = (* "," *) token env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_spec_property env x
      ))
    | None -> R.Option None)
  in
  let v4 = (* "]" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

let map_enum_signature (env : env) ((v1, v2, v3, v4) : CST.enum_signature) =
  let v1 = (* "enum" *) token env v1 in
  let v2 =
    (* pattern (`)?[a-zA-Z_][0-9a-zA-Z_]*(`)? *) token env v2
  in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_type_parameters env x
      ))
    | None -> R.Option None)
  in
  let v4 =
    (match v4 with
    | Some x -> R.Option (Some (
        map_ability_decls env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3; v4]

let map_struct_signature (env : env) ((v1, v2, v3, v4) : CST.struct_signature) =
  let v1 = (* "struct" *) token env v1 in
  let v2 =
    (* pattern (`)?[a-zA-Z_][0-9a-zA-Z_]*(`)? *) token env v2
  in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_type_parameters env x
      ))
    | None -> R.Option None)
  in
  let v4 =
    (match v4 with
    | Some x -> R.Option (Some (
        map_ability_decls env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3; v4]

let map_spec_apply_pattern (env : env) ((v1, v2, v3) : CST.spec_apply_pattern) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        (match x with
        | `Public tok -> R.Case ("Public",
            (* "public" *) token env tok
          )
        | `Inte tok -> R.Case ("Inte",
            (* "internal" *) token env tok
          )
        )
      ))
    | None -> R.Option None)
  in
  let v2 = (* pattern [0-9a-zA-Z_*]+ *) token env v2 in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_type_parameters env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3]

let map_module_access (env : env) (x : CST.module_access) =
  (match x with
  | `DOLLAR_id (v1, v2) -> R.Case ("DOLLAR_id",
      let v1 = (* "$" *) token env v1 in
      let v2 =
        (* pattern (`)?[a-zA-Z_][0-9a-zA-Z_]*(`)? *) token env v2
      in
      R.Tuple [v1; v2]
    )
  | `Rese_id x -> R.Case ("Rese_id",
      map_reserved_identifier env x
    )
  | `Id tok -> R.Case ("Id",
      (* pattern (`)?[a-zA-Z_][0-9a-zA-Z_]*(`)? *) token env tok
    )
  | `Module_id_COLONCOLON_id (v1, v2, v3) -> R.Case ("Module_id_COLONCOLON_id",
      let v1 =
        (* pattern (`)?[a-zA-Z_][0-9a-zA-Z_]*(`)? *) token env v1
      in
      let v2 = (* "::" *) token env v2 in
      let v3 =
        (* pattern (`)?[a-zA-Z_][0-9a-zA-Z_]*(`)? *) token env v3
      in
      R.Tuple [v1; v2; v3]
    )
  | `Module_iden_COLONCOLON_id (v1, v2, v3) -> R.Case ("Module_iden_COLONCOLON_id",
      let v1 = map_module_identity env v1 in
      let v2 = (* "::" *) token env v2 in
      let v3 =
        (* pattern (`)?[a-zA-Z_][0-9a-zA-Z_]*(`)? *) token env v3
      in
      R.Tuple [v1; v2; v3]
    )
  )

let map_friend_access (env : env) (x : CST.friend_access) =
  (match x with
  | `Id tok -> R.Case ("Id",
      (* pattern (`)?[a-zA-Z_][0-9a-zA-Z_]*(`)? *) token env tok
    )
  | `Module_iden x -> R.Case ("Module_iden",
      map_module_identity env x
    )
  )

let map_use_module_member (env : env) ((v1, v2, v3) : CST.use_module_member) =
  let v1 = map_module_identity env v1 in
  let v2 = (* "::" *) token env v2 in
  let v3 = map_use_member env v3 in
  R.Tuple [v1; v2; v3]

let map_use_module_members (env : env) (x : CST.use_module_members) =
  (match x with
  | `Choice_num_lit_COLONCOLON_LCURL_use_member_rep_COMMA_use_member_opt_COMMA_RCURL (v1, v2, v3, v4, v5, v6, v7) -> R.Case ("Choice_num_lit_COLONCOLON_LCURL_use_member_rep_COMMA_use_member_opt_COMMA_RCURL",
      let v1 = map_anon_choice_num_lit_a33e50c env v1 in
      let v2 = (* "::" *) token env v2 in
      let v3 = (* "{" *) token env v3 in
      let v4 = map_use_member env v4 in
      let v5 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = (* "," *) token env v1 in
          let v2 = map_use_member env v2 in
          R.Tuple [v1; v2]
        ) v5)
      in
      let v6 =
        (match v6 with
        | Some tok -> R.Option (Some (
            (* "," *) token env tok
          ))
        | None -> R.Option None)
      in
      let v7 = (* "}" *) token env v7 in
      R.Tuple [v1; v2; v3; v4; v5; v6; v7]
    )
  | `Module_iden_COLONCOLON_LCURL_use_member_rep_COMMA_use_member_opt_COMMA_RCURL (v1, v2, v3, v4, v5, v6, v7) -> R.Case ("Module_iden_COLONCOLON_LCURL_use_member_rep_COMMA_use_member_opt_COMMA_RCURL",
      let v1 = map_module_identity env v1 in
      let v2 = (* "::" *) token env v2 in
      let v3 = (* "{" *) token env v3 in
      let v4 = map_use_member env v4 in
      let v5 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = (* "," *) token env v1 in
          let v2 = map_use_member env v2 in
          R.Tuple [v1; v2]
        ) v5)
      in
      let v6 =
        (match v6 with
        | Some tok -> R.Option (Some (
            (* "," *) token env tok
          ))
        | None -> R.Option None)
      in
      let v7 = (* "}" *) token env v7 in
      R.Tuple [v1; v2; v3; v4; v5; v6; v7]
    )
  )

let map_use_module (env : env) ((v1, v2) : CST.use_module) =
  let v1 = map_module_identity env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = (* "as" *) token env v1 in
        let v2 =
          (* pattern (`)?[a-zA-Z_][0-9a-zA-Z_]*(`)? *) token env v2
        in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2]

let map_spec_block_target (env : env) (x : CST.spec_block_target) =
  (match x with
  | `Id tok -> R.Case ("Id",
      (* pattern (`)?[a-zA-Z_][0-9a-zA-Z_]*(`)? *) token env tok
    )
  | `Module tok -> R.Case ("Module",
      (* "module" *) token env tok
    )
  | `Spec_blk_target_schema (v1, v2, v3) -> R.Case ("Spec_blk_target_schema",
      let v1 = (* "schema" *) token env v1 in
      let v2 =
        (* pattern (`)?[a-zA-Z_][0-9a-zA-Z_]*(`)? *) token env v2
      in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_type_parameters env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3]
    )
  )

let map_anon_choice_module_access_0e4603e (env : env) (x : CST.anon_choice_module_access_0e4603e) =
  (match x with
  | `Module_access x -> R.Case ("Module_access",
      map_module_access env x
    )
  | `Lit_value x -> R.Case ("Lit_value",
      map_literal_value env x
    )
  )

let map_macro_module_access (env : env) ((v1, v2) : CST.macro_module_access) =
  let v1 = map_module_access env v1 in
  let v2 = (* "!" *) token env v2 in
  R.Tuple [v1; v2]

let map_use_fun (env : env) ((v1, v2, v3, v4, v5, v6) : CST.use_fun) =
  let v1 = (* "fun" *) token env v1 in
  let v2 = map_module_access env v2 in
  let v3 = (* "as" *) token env v3 in
  let v4 = map_module_access env v4 in
  let v5 = (* "." *) token env v5 in
  let v6 =
    (* pattern (`)?[a-zA-Z_][0-9a-zA-Z_]*(`)? *) token env v6
  in
  R.Tuple [v1; v2; v3; v4; v5; v6]

let rec map_function_type_parameters (env : env) ((v1, v2, v3, v4) : CST.function_type_parameters) =
  let v1 = (* "|" *) token env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = map_type_ env v1 in
      let v2 = (* "," *) token env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_type_ env x
      ))
    | None -> R.Option None)
  in
  let v4 = (* "|" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_type_ (env : env) (x : CST.type_) =
  (match x with
  | `Apply_type (v1, v2) -> R.Case ("Apply_type",
      let v1 = map_module_access env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_type_arguments env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2]
    )
  | `Ref_type (v1, v2) -> R.Case ("Ref_type",
      let v1 = map_anon_choice_AMP_c6caa5d env v1 in
      let v2 = map_type_ env v2 in
      R.Tuple [v1; v2]
    )
  | `Tuple_type (v1, v2, v3, v4) -> R.Case ("Tuple_type",
      let v1 = (* "(" *) token env v1 in
      let v2 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = map_type_ env v1 in
          let v2 = (* "," *) token env v2 in
          R.Tuple [v1; v2]
        ) v2)
      in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_type_ env x
          ))
        | None -> R.Option None)
      in
      let v4 = (* ")" *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Func_type (v1, v2) -> R.Case ("Func_type",
      let v1 = map_function_type_parameters env v1 in
      let v2 =
        (match v2 with
        | Some (v1, v2) -> R.Option (Some (
            let v1 = (* "->" *) token env v1 in
            let v2 = map_type_ env v2 in
            R.Tuple [v1; v2]
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2]
    )
  | `Prim_type x -> R.Case ("Prim_type",
      map_primitive_type env x
    )
  )

and map_type_arguments (env : env) ((v1, v2, v3, v4, v5) : CST.type_arguments) =
  let v1 = (* "<" *) token env v1 in
  let v2 = map_type_ env v2 in
  let v3 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_type_ env v2 in
      R.Tuple [v1; v2]
    ) v3)
  in
  let v4 =
    (match v4 with
    | Some tok -> R.Option (Some (
        (* "," *) token env tok
      ))
    | None -> R.Option None)
  in
  let v5 = (* ">" *) token env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

let map_anon_choice_lit_value_bd85d4e (env : env) (x : CST.anon_choice_lit_value_bd85d4e) =
  (match x with
  | `Lit_value x -> R.Case ("Lit_value",
      map_literal_value env x
    )
  | `Module_access x -> R.Case ("Module_access",
      map_module_access env x
    )
  )

let map_friend_declaration (env : env) ((v1, v2, v3) : CST.friend_declaration) =
  let v1 = (* "friend" *) token env v1 in
  let v2 = map_friend_access env v2 in
  let v3 = (* ";" *) token env v3 in
  R.Tuple [v1; v2; v3]

let map_use_declaration (env : env) ((v1, v2, v3, v4) : CST.use_declaration) =
  let v1 =
    (match v1 with
    | Some tok -> R.Option (Some (
        (* "public" *) token env tok
      ))
    | None -> R.Option None)
  in
  let v2 = (* "use" *) token env v2 in
  let v3 =
    (match v3 with
    | `Use_fun x -> R.Case ("Use_fun",
        map_use_fun env x
      )
    | `Use_module x -> R.Case ("Use_module",
        map_use_module env x
      )
    | `Use_module_member x -> R.Case ("Use_module_member",
        map_use_module_member env x
      )
    | `Use_module_members x -> R.Case ("Use_module_members",
        map_use_module_members env x
      )
    )
  in
  let v4 = (* ";" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

let map_function_parameter (env : env) ((v1, v2, v3, v4) : CST.function_parameter) =
  let v1 =
    (match v1 with
    | Some tok -> R.Option (Some (
        (* "mut" *) token env tok
      ))
    | None -> R.Option None)
  in
  let v2 =
    (match v2 with
    | `Var_id tok -> R.Case ("Var_id",
        (* pattern (`)?[a-zA-Z_][0-9a-zA-Z_]*(`)? *) token env tok
      )
    | `DOLLAR_var_id (v1, v2) -> R.Case ("DOLLAR_var_id",
        let v1 = (* "$" *) token env v1 in
        let v2 =
          (* pattern (`)?[a-zA-Z_][0-9a-zA-Z_]*(`)? *) token env v2
        in
        R.Tuple [v1; v2]
      )
    )
  in
  let v3 = (* ":" *) token env v3 in
  let v4 = map_type_ env v4 in
  R.Tuple [v1; v2; v3; v4]

let map_ret_type (env : env) ((v1, v2) : CST.ret_type) =
  let v1 = (* ":" *) token env v1 in
  let v2 = map_type_ env v2 in
  R.Tuple [v1; v2]

let rec map_bind (env : env) (x : CST.bind) =
  (match x with
  | `Opt_mut_var_id (v1, v2) -> R.Case ("Opt_mut_var_id",
      let v1 =
        (match v1 with
        | Some tok -> R.Option (Some (
            (* "mut" *) token env tok
          ))
        | None -> R.Option None)
      in
      let v2 =
        (* pattern (`)?[a-zA-Z_][0-9a-zA-Z_]*(`)? *) token env v2
      in
      R.Tuple [v1; v2]
    )
  | `Bind_unpack (v1, v2, v3) -> R.Case ("Bind_unpack",
      let v1 = map_module_access env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_type_arguments env x
          ))
        | None -> R.Option None)
      in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_bind_fields env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3]
    )
  | `Var_id_AT_bind (v1, v2, v3) -> R.Case ("Var_id_AT_bind",
      let v1 =
        (* pattern (`)?[a-zA-Z_][0-9a-zA-Z_]*(`)? *) token env v1
      in
      let v2 = (* "@" *) token env v2 in
      let v3 = map_bind env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_bind_field (env : env) ((v1, v2, v3) : CST.bind_field) =
  let v1 =
    (match v1 with
    | Some tok -> R.Option (Some (
        (* "mut" *) token env tok
      ))
    | None -> R.Option None)
  in
  let v2 =
    (* pattern (`)?[a-zA-Z_][0-9a-zA-Z_]*(`)? *) token env v2
  in
  let v3 =
    (match v3 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = (* ":" *) token env v1 in
        let v2 = map_bind env v2 in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3]

and map_bind_fields (env : env) (x : CST.bind_fields) =
  (match x with
  | `Bind_posi_fields (v1, v2, v3, v4) -> R.Case ("Bind_posi_fields",
      let v1 = (* "(" *) token env v1 in
      let v2 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = map_bind_field env v1 in
          let v2 = (* "," *) token env v2 in
          R.Tuple [v1; v2]
        ) v2)
      in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_bind_field env x
          ))
        | None -> R.Option None)
      in
      let v4 = (* ")" *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Bind_named_fields (v1, v2, v3, v4) -> R.Case ("Bind_named_fields",
      let v1 = (* "{" *) token env v1 in
      let v2 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = map_bind_field env v1 in
          let v2 = (* "," *) token env v2 in
          R.Tuple [v1; v2]
        ) v2)
      in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_bind_field env x
          ))
        | None -> R.Option None)
      in
      let v4 = (* "}" *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  )

let map_field_annotation (env : env) ((v1, v2, v3) : CST.field_annotation) =
  let v1 =
    (* pattern (`)?[a-zA-Z_][0-9a-zA-Z_]*(`)? *) token env v1
  in
  let v2 = (* ":" *) token env v2 in
  let v3 = map_type_ env v3 in
  R.Tuple [v1; v2; v3]

let map_annotation_item (env : env) (x : CST.annotation_item) =
  (match x with
  | `Id tok -> R.Case ("Id",
      (* pattern (`)?[a-zA-Z_][0-9a-zA-Z_]*(`)? *) token env tok
    )
  | `Id_EQ_lit_value (v1, v2, v3) -> R.Case ("Id_EQ_lit_value",
      let v1 =
        (* pattern (`)?[a-zA-Z_][0-9a-zA-Z_]*(`)? *) token env v1
      in
      let v2 = (* "=" *) token env v2 in
      let v3 = map_literal_value env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Id_LPAR_choice_lit_value_rep_COMMA_choice_lit_value_opt_COMMA_RPAR (v1, v2, v3, v4, v5, v6) -> R.Case ("Id_LPAR_choice_lit_value_rep_COMMA_choice_lit_value_opt_COMMA_RPAR",
      let v1 =
        (* pattern (`)?[a-zA-Z_][0-9a-zA-Z_]*(`)? *) token env v1
      in
      let v2 = (* "(" *) token env v2 in
      let v3 = map_anon_choice_lit_value_bd85d4e env v3 in
      let v4 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = (* "," *) token env v1 in
          let v2 = map_anon_choice_lit_value_bd85d4e env v2 in
          R.Tuple [v1; v2]
        ) v4)
      in
      let v5 =
        (match v5 with
        | Some tok -> R.Option (Some (
            (* "," *) token env tok
          ))
        | None -> R.Option None)
      in
      let v6 = (* ")" *) token env v6 in
      R.Tuple [v1; v2; v3; v4; v5; v6]
    )
  | `Id_LPAR_id_EQ_choice_module_access_rep_COMMA_id_EQ_choice_module_access_opt_COMMA_RPAR (v1, v2, v3, v4, v5, v6, v7, v8) -> R.Case ("Id_LPAR_id_EQ_choice_module_access_rep_COMMA_id_EQ_choice_module_access_opt_COMMA_RPAR",
      let v1 =
        (* pattern (`)?[a-zA-Z_][0-9a-zA-Z_]*(`)? *) token env v1
      in
      let v2 = (* "(" *) token env v2 in
      let v3 =
        (* pattern (`)?[a-zA-Z_][0-9a-zA-Z_]*(`)? *) token env v3
      in
      let v4 = (* "=" *) token env v4 in
      let v5 = map_anon_choice_module_access_0e4603e env v5 in
      let v6 =
        R.List (List.map (fun (v1, v2, v3, v4) ->
          let v1 = (* "," *) token env v1 in
          let v2 =
            (* pattern (`)?[a-zA-Z_][0-9a-zA-Z_]*(`)? *) token env v2
          in
          let v3 = (* "=" *) token env v3 in
          let v4 = map_anon_choice_module_access_0e4603e env v4 in
          R.Tuple [v1; v2; v3; v4]
        ) v6)
      in
      let v7 =
        (match v7 with
        | Some tok -> R.Option (Some (
            (* "," *) token env tok
          ))
        | None -> R.Option None)
      in
      let v8 = (* ")" *) token env v8 in
      R.Tuple [v1; v2; v3; v4; v5; v6; v7; v8]
    )
  )

let map_function_parameters (env : env) ((v1, v2, v3, v4) : CST.function_parameters) =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = map_function_parameter env v1 in
      let v2 = (* "," *) token env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_function_parameter env x
      ))
    | None -> R.Option None)
  in
  let v4 = (* ")" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

let map_bind_list (env : env) (x : CST.bind_list) =
  (match x with
  | `Bind x -> R.Case ("Bind",
      map_bind env x
    )
  | `LPAR_rep_bind_COMMA_opt_bind_RPAR (v1, v2, v3, v4) -> R.Case ("LPAR_rep_bind_COMMA_opt_bind_RPAR",
      let v1 = (* "(" *) token env v1 in
      let v2 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = map_bind env v1 in
          let v2 = (* "," *) token env v2 in
          R.Tuple [v1; v2]
        ) v2)
      in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_bind env x
          ))
        | None -> R.Option None)
      in
      let v4 = (* ")" *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  )

let map_lambda_bindings (env : env) ((v1, v2, v3, v4) : CST.lambda_bindings) =
  let v1 = (* "|" *) token env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = map_bind env v1 in
      let v2 = (* "," *) token env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_bind env x
      ))
    | None -> R.Option None)
  in
  let v4 = (* "|" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

let map_spec_function_signature (env : env) ((v1, v2, v3, v4) : CST.spec_function_signature) =
  let v1 =
    (* pattern (`)?[a-zA-Z_][0-9a-zA-Z_]*(`)? *) token env v1
  in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_type_parameters env x
      ))
    | None -> R.Option None)
  in
  let v3 = map_function_parameters env v3 in
  let v4 = map_ret_type env v4 in
  R.Tuple [v1; v2; v3; v4]

let map_function_signature (env : env) ((v1, v2, v3, v4, v5, v6, v7) : CST.function_signature) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        map_modifier env x
      ))
    | None -> R.Option None)
  in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_modifier env x
      ))
    | None -> R.Option None)
  in
  let v3 = (* "fun" *) token env v3 in
  let v4 =
    (* pattern (`)?[a-zA-Z_][0-9a-zA-Z_]*(`)? *) token env v4
  in
  let v5 =
    (match v5 with
    | Some x -> R.Option (Some (
        map_type_parameters env x
      ))
    | None -> R.Option None)
  in
  let v6 = map_function_parameters env v6 in
  let v7 =
    (match v7 with
    | Some x -> R.Option (Some (
        map_ret_type env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3; v4; v5; v6; v7]

let map_macro_signature (env : env) ((v1, v2, v3, v4, v5, v6) : CST.macro_signature) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        map_modifier env x
      ))
    | None -> R.Option None)
  in
  let v2 = (* "fun" *) token env v2 in
  let v3 =
    (* pattern (`)?[a-zA-Z_][0-9a-zA-Z_]*(`)? *) token env v3
  in
  let v4 =
    (match v4 with
    | Some x -> R.Option (Some (
        map_type_parameters env x
      ))
    | None -> R.Option None)
  in
  let v5 = map_function_parameters env v5 in
  let v6 =
    (match v6 with
    | Some x -> R.Option (Some (
        map_ret_type env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3; v4; v5; v6]

let map_datatype_fields (env : env) (x : CST.datatype_fields) =
  (match x with
  | `Posi_fields (v1, v2, v3, v4) -> R.Case ("Posi_fields",
      let v1 = (* "(" *) token env v1 in
      let v2 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = map_type_ env v1 in
          let v2 = (* "," *) token env v2 in
          R.Tuple [v1; v2]
        ) v2)
      in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_type_ env x
          ))
        | None -> R.Option None)
      in
      let v4 = (* ")" *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Named_fields (v1, v2, v3, v4) -> R.Case ("Named_fields",
      let v1 = (* "{" *) token env v1 in
      let v2 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = map_field_annotation env v1 in
          let v2 = (* "," *) token env v2 in
          R.Tuple [v1; v2]
        ) v2)
      in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_field_annotation env x
          ))
        | None -> R.Option None)
      in
      let v4 = (* "}" *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  )

let map_variant (env : env) ((v1, v2) : CST.variant) =
  let v1 =
    (* pattern (`)?[a-zA-Z_][0-9a-zA-Z_]*(`)? *) token env v1
  in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_datatype_fields env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2]

let rec map_arg_list (env : env) ((v1, v2, v3, v4) : CST.arg_list) =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = map_expression env v1 in
      let v2 = (* "," *) token env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_expression env x
      ))
    | None -> R.Option None)
  in
  let v4 = (* ")" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_binary_expression (env : env) (x : CST.binary_expression) =
  (match x with
  | `Bin_oper_EQEQGT_bin_oper (v1, v2, v3) -> R.Case ("Bin_oper_EQEQGT_bin_oper",
      let v1 = map_binary_operand env v1 in
      let v2 = (* "==>" *) token env v2 in
      let v3 = map_binary_operand env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Bin_oper_BARBAR_bin_oper (v1, v2, v3) -> R.Case ("Bin_oper_BARBAR_bin_oper",
      let v1 = map_binary_operand env v1 in
      let v2 = (* "||" *) token env v2 in
      let v3 = map_binary_operand env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Bin_oper_AMPAMP_bin_oper (v1, v2, v3) -> R.Case ("Bin_oper_AMPAMP_bin_oper",
      let v1 = map_binary_operand env v1 in
      let v2 = (* "&&" *) token env v2 in
      let v3 = map_binary_operand env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Bin_oper_EQEQ_bin_oper (v1, v2, v3) -> R.Case ("Bin_oper_EQEQ_bin_oper",
      let v1 = map_binary_operand env v1 in
      let v2 = (* "==" *) token env v2 in
      let v3 = map_binary_operand env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Bin_oper_BANGEQ_bin_oper (v1, v2, v3) -> R.Case ("Bin_oper_BANGEQ_bin_oper",
      let v1 = map_binary_operand env v1 in
      let v2 = (* "!=" *) token env v2 in
      let v3 = map_binary_operand env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Bin_oper_LT_bin_oper (v1, v2, v3) -> R.Case ("Bin_oper_LT_bin_oper",
      let v1 = map_binary_operand env v1 in
      let v2 = (* "<" *) token env v2 in
      let v3 = map_binary_operand env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Bin_oper_GT_bin_oper (v1, v2, v3) -> R.Case ("Bin_oper_GT_bin_oper",
      let v1 = map_binary_operand env v1 in
      let v2 = (* ">" *) token env v2 in
      let v3 = map_binary_operand env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Bin_oper_LTEQ_bin_oper (v1, v2, v3) -> R.Case ("Bin_oper_LTEQ_bin_oper",
      let v1 = map_binary_operand env v1 in
      let v2 = (* "<=" *) token env v2 in
      let v3 = map_binary_operand env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Bin_oper_GTEQ_bin_oper (v1, v2, v3) -> R.Case ("Bin_oper_GTEQ_bin_oper",
      let v1 = map_binary_operand env v1 in
      let v2 = (* ">=" *) token env v2 in
      let v3 = map_binary_operand env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Bin_oper_DOTDOT_bin_oper (v1, v2, v3) -> R.Case ("Bin_oper_DOTDOT_bin_oper",
      let v1 = map_binary_operand env v1 in
      let v2 = (* ".." *) token env v2 in
      let v3 = map_binary_operand env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Bin_oper_BAR_bin_oper (v1, v2, v3) -> R.Case ("Bin_oper_BAR_bin_oper",
      let v1 = map_binary_operand env v1 in
      let v2 = (* "|" *) token env v2 in
      let v3 = map_binary_operand env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Bin_oper_HAT_bin_oper (v1, v2, v3) -> R.Case ("Bin_oper_HAT_bin_oper",
      let v1 = map_binary_operand env v1 in
      let v2 = (* "^" *) token env v2 in
      let v3 = map_binary_operand env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Bin_oper_AMP_bin_oper (v1, v2, v3) -> R.Case ("Bin_oper_AMP_bin_oper",
      let v1 = map_binary_operand env v1 in
      let v2 = (* "&" *) token env v2 in
      let v3 = map_binary_operand env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Bin_oper_LTLT_bin_oper (v1, v2, v3) -> R.Case ("Bin_oper_LTLT_bin_oper",
      let v1 = map_binary_operand env v1 in
      let v2 = (* "<<" *) token env v2 in
      let v3 = map_binary_operand env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Bin_oper_GTGT_bin_oper (v1, v2, v3) -> R.Case ("Bin_oper_GTGT_bin_oper",
      let v1 = map_binary_operand env v1 in
      let v2 = (* ">>" *) token env v2 in
      let v3 = map_binary_operand env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Bin_oper_PLUS_bin_oper (v1, v2, v3) -> R.Case ("Bin_oper_PLUS_bin_oper",
      let v1 = map_binary_operand env v1 in
      let v2 = (* "+" *) token env v2 in
      let v3 = map_binary_operand env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Bin_oper_DASH_bin_oper (v1, v2, v3) -> R.Case ("Bin_oper_DASH_bin_oper",
      let v1 = map_binary_operand env v1 in
      let v2 = (* "-" *) token env v2 in
      let v3 = map_binary_operand env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Bin_oper_STAR_bin_oper (v1, v2, v3) -> R.Case ("Bin_oper_STAR_bin_oper",
      let v1 = map_binary_operand env v1 in
      let v2 = (* "*" *) token env v2 in
      let v3 = map_binary_operand env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Bin_oper_SLASH_bin_oper (v1, v2, v3) -> R.Case ("Bin_oper_SLASH_bin_oper",
      let v1 = map_binary_operand env v1 in
      let v2 = (* "/" *) token env v2 in
      let v3 = map_binary_operand env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Bin_oper_PERC_bin_oper (v1, v2, v3) -> R.Case ("Bin_oper_PERC_bin_oper",
      let v1 = map_binary_operand env v1 in
      let v2 = (* "%" *) token env v2 in
      let v3 = map_binary_operand env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_binary_operand (env : env) (x : CST.binary_operand) =
  (match x with
  | `Un_exp x -> R.Case ("Un_exp",
      map_unary_expression env x
    )
  | `Bin_exp x -> R.Case ("Bin_exp",
      map_binary_expression env x
    )
  | `Cast_exp x -> R.Case ("Cast_exp",
      map_cast_expression env x
    )
  )

and map_block (env : env) ((v1, v2, v3, v4, v5) : CST.block) =
  let v1 = (* "{" *) token env v1 in
  let v2 = R.List (List.map (map_use_declaration env) v2) in
  let v3 = R.List (List.map (map_block_item env) v3) in
  let v4 =
    (match v4 with
    | Some x -> R.Option (Some (
        map_expression env x
      ))
    | None -> R.Option None)
  in
  let v5 = (* "}" *) token env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

and map_block_item (env : env) ((v1, v2) : CST.block_item) =
  let v1 =
    (match v1 with
    | `Exp x -> R.Case ("Exp",
        map_expression env x
      )
    | `Let_stmt x -> R.Case ("Let_stmt",
        map_let_statement env x
      )
    )
  in
  let v2 = (* ";" *) token env v2 in
  R.Tuple [v1; v2]

and map_cast_expression (env : env) ((v1, v2, v3) : CST.cast_expression) =
  let v1 = map_expression env v1 in
  let v2 = (* "as" *) token env v2 in
  let v3 = map_type_ env v3 in
  R.Tuple [v1; v2; v3]

and map_exp_field (env : env) ((v1, v2) : CST.exp_field) =
  let v1 =
    (* pattern (`)?[a-zA-Z_][0-9a-zA-Z_]*(`)? *) token env v1
  in
  let v2 =
    (match v2 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = (* ":" *) token env v1 in
        let v2 = map_expression env v2 in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2]

and map_expression (env : env) (x : CST.expression) =
  (match x with
  | `Lambda_exp (v1, v2, v3) -> R.Case ("Lambda_exp",
      let v1 = map_lambda_bindings env v1 in
      let v2 =
        (match v2 with
        | Some (v1, v2) -> R.Option (Some (
            let v1 = (* "->" *) token env v1 in
            let v2 = map_type_ env v2 in
            R.Tuple [v1; v2]
          ))
        | None -> R.Option None)
      in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `If_exp x -> R.Case ("If_exp",
      map_if_expression env x
    )
  | `While_exp (v1, v2, v3, v4, v5) -> R.Case ("While_exp",
      let v1 = (* "while" *) token env v1 in
      let v2 = (* "(" *) token env v2 in
      let v3 = map_expression env v3 in
      let v4 = (* ")" *) token env v4 in
      let v5 = map_expression env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `Loop_exp (v1, v2) -> R.Case ("Loop_exp",
      let v1 = (* "loop" *) token env v1 in
      let v2 = map_expression env v2 in
      R.Tuple [v1; v2]
    )
  | `Ret_exp (v1, v2, v3) -> R.Case ("Ret_exp",
      let v1 = (* "return" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_label env x
          ))
        | None -> R.Option None)
      in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            (match x with
            | `Exp_term x -> R.Case ("Exp_term",
                map_expression_term env x
              )
            | `Exp x -> R.Case ("Exp",
                map_expression env x
              )
            )
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3]
    )
  | `Abort_exp (v1, v2) -> R.Case ("Abort_exp",
      let v1 = (* "abort" *) token env v1 in
      let v2 = map_expression env v2 in
      R.Tuple [v1; v2]
    )
  | `Assign_exp (v1, v2, v3) -> R.Case ("Assign_exp",
      let v1 = map_unary_expression env v1 in
      let v2 = (* "=" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Un_exp x -> R.Case ("Un_exp",
      map_unary_expression env x
    )
  | `Bin_exp x -> R.Case ("Bin_exp",
      map_binary_expression env x
    )
  | `Cast_exp x -> R.Case ("Cast_exp",
      map_cast_expression env x
    )
  | `Quan_exp (v1, v2, v3, v4, v5) -> R.Case ("Quan_exp",
      let v1 = map_reserved_identifier env v1 in
      let v2 = map_quantifier_bindings env v2 in
      let v3 =
        (match v3 with
        | Some (v1, v2) -> R.Option (Some (
            let v1 = (* "where" *) token env v1 in
            let v2 = map_expression env v2 in
            R.Tuple [v1; v2]
          ))
        | None -> R.Option None)
      in
      let v4 = (* ":" *) token env v4 in
      let v5 = map_expression env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `Iden_exp (v1, v2) -> R.Case ("Iden_exp",
      let v1 = map_block_identifier env v1 in
      let v2 = map_expression env v2 in
      R.Tuple [v1; v2]
    )
  | `Match_exp (v1, v2, v3, v4, v5) -> R.Case ("Match_exp",
      let v1 = (* "match" *) token env v1 in
      let v2 = (* "(" *) token env v2 in
      let v3 = map_expression env v3 in
      let v4 = (* ")" *) token env v4 in
      let v5 = map_match_body env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `Vec_exp (v1, v2, v3, v4) -> R.Case ("Vec_exp",
      let v1 =
        (match v1 with
        | `Vect tok -> R.Case ("Vect",
            (* "vector[" *) token env tok
          )
        | `Vect_type_rep_COMMA_type_opt_COMMA_GT_LBRACK (v1, v2, v3, v4, v5, v6) -> R.Case ("Vect_type_rep_COMMA_type_opt_COMMA_GT_LBRACK",
            let v1 = (* "vector<" *) token env v1 in
            let v2 = map_type_ env v2 in
            let v3 =
              R.List (List.map (fun (v1, v2) ->
                let v1 = (* "," *) token env v1 in
                let v2 = map_type_ env v2 in
                R.Tuple [v1; v2]
              ) v3)
            in
            let v4 =
              (match v4 with
              | Some tok -> R.Option (Some (
                  (* "," *) token env tok
                ))
              | None -> R.Option None)
            in
            let v5 = (* ">" *) token env v5 in
            let v6 = (* "[" *) token env v6 in
            R.Tuple [v1; v2; v3; v4; v5; v6]
          )
        )
      in
      let v2 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = map_expression env v1 in
          let v2 = (* "," *) token env v2 in
          R.Tuple [v1; v2]
        ) v2)
      in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_expression env x
          ))
        | None -> R.Option None)
      in
      let v4 = (* "]" *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  )

and map_expression_term (env : env) (x : CST.expression_term) =
  (match x with
  | `Brk_exp (v1, v2, v3) -> R.Case ("Brk_exp",
      let v1 = (* "break" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_label env x
          ))
        | None -> R.Option None)
      in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_expression_term env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3]
    )
  | `Cont_exp (v1, v2) -> R.Case ("Cont_exp",
      let v1 = (* "continue" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_label env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2]
    )
  | `Name_exp (v1, v2) -> R.Case ("Name_exp",
      let v1 = map_module_access env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_type_arguments env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2]
    )
  | `Call_exp (v1, v2, v3) -> R.Case ("Call_exp",
      let v1 = map_module_access env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_type_arguments env x
          ))
        | None -> R.Option None)
      in
      let v3 = map_arg_list env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Macro_call_exp (v1, v2, v3) -> R.Case ("Macro_call_exp",
      let v1 = map_macro_module_access env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_type_arguments env x
          ))
        | None -> R.Option None)
      in
      let v3 = map_arg_list env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Pack_exp (v1, v2, v3) -> R.Case ("Pack_exp",
      let v1 = map_module_access env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_type_arguments env x
          ))
        | None -> R.Option None)
      in
      let v3 = map_field_initialize_list env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Lit_value x -> R.Case ("Lit_value",
      map_literal_value env x
    )
  | `Unit_exp (v1, v2) -> R.Case ("Unit_exp",
      let v1 = (* "(" *) token env v1 in
      let v2 = (* ")" *) token env v2 in
      R.Tuple [v1; v2]
    )
  | `Exp_list (v1, v2, v3, v4, v5) -> R.Case ("Exp_list",
      let v1 = (* "(" *) token env v1 in
      let v2 = map_expression env v2 in
      let v3 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = (* "," *) token env v1 in
          let v2 = map_expression env v2 in
          R.Tuple [v1; v2]
        ) v3)
      in
      let v4 =
        (match v4 with
        | Some tok -> R.Option (Some (
            (* "," *) token env tok
          ))
        | None -> R.Option None)
      in
      let v5 = (* ")" *) token env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `Anno_exp (v1, v2, v3, v4, v5) -> R.Case ("Anno_exp",
      let v1 = (* "(" *) token env v1 in
      let v2 = map_expression env v2 in
      let v3 = (* ":" *) token env v3 in
      let v4 = map_type_ env v4 in
      let v5 = (* ")" *) token env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `Blk x -> R.Case ("Blk",
      map_block env x
    )
  | `Spec_blk x -> R.Case ("Spec_blk",
      map_spec_block env x
    )
  | `If_exp x -> R.Case ("If_exp",
      map_if_expression env x
    )
  | `Dot_exp (v1, v2, v3) -> R.Case ("Dot_exp",
      let v1 = map_expression_term env v1 in
      let v2 = (* "." *) token env v2 in
      let v3 = map_expression_term env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Index_exp (v1, v2, v3, v4) -> R.Case ("Index_exp",
      let v1 = map_expression_term env v1 in
      let v2 = (* "[" *) token env v2 in
      let v3 = map_expression env v3 in
      let v4 = (* "]" *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  )

and map_field_initialize_list (env : env) ((v1, v2, v3, v4) : CST.field_initialize_list) =
  let v1 = (* "{" *) token env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = map_exp_field env v1 in
      let v2 = (* "," *) token env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_exp_field env x
      ))
    | None -> R.Option None)
  in
  let v4 = (* "}" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_if_expression (env : env) ((v1, v2, v3, v4, v5, v6) : CST.if_expression) =
  let v1 = (* "if" *) token env v1 in
  let v2 = (* "(" *) token env v2 in
  let v3 = map_expression env v3 in
  let v4 = (* ")" *) token env v4 in
  let v5 = map_expression env v5 in
  let v6 =
    (match v6 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = (* "else" *) token env v1 in
        let v2 = map_expression env v2 in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3; v4; v5; v6]

and map_let_statement (env : env) ((v1, v2, v3, v4) : CST.let_statement) =
  let v1 = (* "let" *) token env v1 in
  let v2 = map_bind_list env v2 in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_ret_type env x
      ))
    | None -> R.Option None)
  in
  let v4 =
    (match v4 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = (* "=" *) token env v1 in
        let v2 = map_expression env v2 in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3; v4]

and map_match_arm (env : env) ((v1, v2, v3, v4) : CST.match_arm) =
  let v1 = map_bind_list env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = (* "if" *) token env v1 in
        let v2 = map_expression env v2 in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  let v3 = (* "=>" *) token env v3 in
  let v4 = map_expression env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_match_body (env : env) ((v1, v2, v3, v4) : CST.match_body) =
  let v1 = (* "{" *) token env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = map_match_arm env v1 in
      let v2 = (* "," *) token env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_match_arm env x
      ))
    | None -> R.Option None)
  in
  let v4 = (* "}" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_quantifier_binding (env : env) (x : CST.quantifier_binding) =
  (match x with
  | `Id_COLON_type (v1, v2, v3) -> R.Case ("Id_COLON_type",
      let v1 =
        (* pattern (`)?[a-zA-Z_][0-9a-zA-Z_]*(`)? *) token env v1
      in
      let v2 = (* ":" *) token env v2 in
      let v3 = map_type_ env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Id_in_exp (v1, v2, v3) -> R.Case ("Id_in_exp",
      let v1 =
        (* pattern (`)?[a-zA-Z_][0-9a-zA-Z_]*(`)? *) token env v1
      in
      let v2 = (* "in" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_quantifier_bindings (env : env) ((v1, v2, v3) : CST.quantifier_bindings) =
  let v1 = map_quantifier_binding env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_quantifier_binding env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  let v3 =
    (match v3 with
    | Some tok -> R.Option (Some (
        (* "," *) token env tok
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3]

and map_spec_block (env : env) ((v1, v2) : CST.spec_block) =
  let v1 = (* "spec" *) token env v1 in
  let v2 =
    (match v2 with
    | `Opt_spec_blk_target_spec_body (v1, v2) -> R.Case ("Opt_spec_blk_target_spec_body",
        let v1 =
          (match v1 with
          | Some x -> R.Option (Some (
              map_spec_block_target env x
            ))
          | None -> R.Option None)
        in
        let v2 = map_spec_body env v2 in
        R.Tuple [v1; v2]
      )
    | `Spec_func x -> R.Case ("Spec_func",
        map_spec_function env x
      )
    )
  in
  R.Tuple [v1; v2]

and map_spec_block_memeber (env : env) (x : CST.spec_block_memeber) =
  (match x with
  | `Spec_inva (v1, v2, v3, v4, v5) -> R.Case ("Spec_inva",
      let v1 = (* "invariant" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            (match x with
            | `Update tok -> R.Case ("Update",
                (* "update" *) token env tok
              )
            | `Pack tok -> R.Case ("Pack",
                (* "pack" *) token env tok
              )
            | `Unpack tok -> R.Case ("Unpack",
                (* "unpack" *) token env tok
              )
            | `Module tok -> R.Case ("Module",
                (* "module" *) token env tok
              )
            )
          ))
        | None -> R.Option None)
      in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_condition_properties env x
          ))
        | None -> R.Option None)
      in
      let v4 = map_expression env v4 in
      let v5 = (* ";" *) token env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `Spec_func x -> R.Case ("Spec_func",
      map_spec_function env x
    )
  | `Spec_cond x -> R.Case ("Spec_cond",
      map_spec_condition env x
    )
  | `Spec_incl (v1, v2, v3) -> R.Case ("Spec_incl",
      let v1 = (* "include" *) token env v1 in
      let v2 = map_expression env v2 in
      let v3 = (* ";" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Spec_apply (v1, v2, v3, v4, v5, v6, v7, v8) -> R.Case ("Spec_apply",
      let v1 = (* "apply" *) token env v1 in
      let v2 = map_expression env v2 in
      let v3 = (* "to" *) token env v3 in
      let v4 = map_spec_apply_pattern env v4 in
      let v5 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = (* "," *) token env v1 in
          let v2 = map_spec_apply_pattern env v2 in
          R.Tuple [v1; v2]
        ) v5)
      in
      let v6 =
        (match v6 with
        | Some tok -> R.Option (Some (
            (* "," *) token env tok
          ))
        | None -> R.Option None)
      in
      let v7 =
        (match v7 with
        | Some (v1, v2, v3, v4) -> R.Option (Some (
            let v1 = (* "except" *) token env v1 in
            let v2 = map_spec_apply_pattern env v2 in
            let v3 =
              R.List (List.map (fun (v1, v2) ->
                let v1 = (* "," *) token env v1 in
                let v2 = map_spec_apply_pattern env v2 in
                R.Tuple [v1; v2]
              ) v3)
            in
            let v4 =
              (match v4 with
              | Some tok -> R.Option (Some (
                  (* "," *) token env tok
                ))
              | None -> R.Option None)
            in
            R.Tuple [v1; v2; v3; v4]
          ))
        | None -> R.Option None)
      in
      let v8 = (* ";" *) token env v8 in
      R.Tuple [v1; v2; v3; v4; v5; v6; v7; v8]
    )
  | `Spec_pragma (v1, v2, v3, v4) -> R.Case ("Spec_pragma",
      let v1 = (* "pragma" *) token env v1 in
      let v2 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = map_spec_property env v1 in
          let v2 = (* "," *) token env v2 in
          R.Tuple [v1; v2]
        ) v2)
      in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_spec_property env x
          ))
        | None -> R.Option None)
      in
      let v4 = (* ";" *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Spec_var (v1, v2, v3, v4, v5, v6) -> R.Case ("Spec_var",
      let v1 =
        (match v1 with
        | Some x -> R.Option (Some (
            (match x with
            | `Global tok -> R.Case ("Global",
                (* "global" *) token env tok
              )
            | `Local tok -> R.Case ("Local",
                (* "local" *) token env tok
              )
            )
          ))
        | None -> R.Option None)
      in
      let v2 =
        (* pattern (`)?[a-zA-Z_][0-9a-zA-Z_]*(`)? *) token env v2
      in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_type_parameters env x
          ))
        | None -> R.Option None)
      in
      let v4 = (* ":" *) token env v4 in
      let v5 = map_type_ env v5 in
      let v6 = (* ";" *) token env v6 in
      R.Tuple [v1; v2; v3; v4; v5; v6]
    )
  | `Spec_let (v1, v2, v3, v4, v5, v6) -> R.Case ("Spec_let",
      let v1 = (* "let" *) token env v1 in
      let v2 =
        (match v2 with
        | Some tok -> R.Option (Some (
            (* "post" *) token env tok
          ))
        | None -> R.Option None)
      in
      let v3 =
        (* pattern (`)?[a-zA-Z_][0-9a-zA-Z_]*(`)? *) token env v3
      in
      let v4 = (* "=" *) token env v4 in
      let v5 = map_expression env v5 in
      let v6 = (* ";" *) token env v6 in
      R.Tuple [v1; v2; v3; v4; v5; v6]
    )
  )

and map_spec_body (env : env) ((v1, v2, v3, v4) : CST.spec_body) =
  let v1 = (* "{" *) token env v1 in
  let v2 = R.List (List.map (map_use_declaration env) v2) in
  let v3 =
    R.List (List.map (map_spec_block_memeber env) v3)
  in
  let v4 = (* "}" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_spec_condition (env : env) (x : CST.spec_condition) =
  (match x with
  | `Spec_cond_ (v1, v2, v3, v4) -> R.Case ("Spec_cond_",
      let v1 =
        (match v1 with
        | `Spec_cond_kind x -> R.Case ("Spec_cond_kind",
            map_spec_condition_kind env x
          )
        | `Requis_opt_module (v1, v2) -> R.Case ("Requis_opt_module",
            let v1 = (* "requires" *) token env v1 in
            let v2 =
              (match v2 with
              | Some tok -> R.Option (Some (
                  (* "module" *) token env tok
                ))
              | None -> R.Option None)
            in
            R.Tuple [v1; v2]
          )
        )
      in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_condition_properties env x
          ))
        | None -> R.Option None)
      in
      let v3 = map_expression env v3 in
      let v4 = (* ";" *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Spec_abort_if (v1, v2, v3, v4, v5) -> R.Case ("Spec_abort_if",
      let v1 = (* "aborts_if" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_condition_properties env x
          ))
        | None -> R.Option None)
      in
      let v3 = map_expression env v3 in
      let v4 =
        (match v4 with
        | Some (v1, v2) -> R.Option (Some (
            let v1 = (* "with" *) token env v1 in
            let v2 = map_expression env v2 in
            R.Tuple [v1; v2]
          ))
        | None -> R.Option None)
      in
      let v5 = (* ";" *) token env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `Spec_abort_with_or_modifs (v1, v2, v3, v4, v5, v6) -> R.Case ("Spec_abort_with_or_modifs",
      let v1 =
        (match v1 with
        | `Aborts_with tok -> R.Case ("Aborts_with",
            (* "aborts_with" *) token env tok
          )
        | `Modifs tok -> R.Case ("Modifs",
            (* "modifies" *) token env tok
          )
        )
      in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_condition_properties env x
          ))
        | None -> R.Option None)
      in
      let v3 = map_expression env v3 in
      let v4 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = (* "," *) token env v1 in
          let v2 = map_expression env v2 in
          R.Tuple [v1; v2]
        ) v4)
      in
      let v5 =
        (match v5 with
        | Some tok -> R.Option (Some (
            (* "," *) token env tok
          ))
        | None -> R.Option None)
      in
      let v6 = (* ";" *) token env v6 in
      R.Tuple [v1; v2; v3; v4; v5; v6]
    )
  )

and map_spec_function (env : env) (x : CST.spec_function) =
  (match x with
  | `Native_spec_func (v1, v2, v3, v4) -> R.Case ("Native_spec_func",
      let v1 = (* "native" *) token env v1 in
      let v2 = (* "fun" *) token env v2 in
      let v3 = map_spec_function_signature env v3 in
      let v4 = (* ";" *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Usual_spec_func (v1, v2, v3) -> R.Case ("Usual_spec_func",
      let v1 = (* "fun" *) token env v1 in
      let v2 = map_spec_function_signature env v2 in
      let v3 = map_block env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Unin_spec_func (v1, v2, v3) -> R.Case ("Unin_spec_func",
      let v1 = (* "fun" *) token env v1 in
      let v2 = map_spec_function_signature env v2 in
      let v3 = (* ";" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_unary_expression (env : env) (x : CST.unary_expression) =
  (match x with
  | `Un_exp_ (v1, v2) -> R.Case ("Un_exp_",
      let v1 = map_unary_op env v1 in
      let v2 = map_expression env v2 in
      R.Tuple [v1; v2]
    )
  | `Borrow_exp (v1, v2) -> R.Case ("Borrow_exp",
      let v1 = map_anon_choice_AMP_c6caa5d env v1 in
      let v2 = map_expression env v2 in
      R.Tuple [v1; v2]
    )
  | `Dere_exp (v1, v2) -> R.Case ("Dere_exp",
      let v1 = (* "*" *) token env v1 in
      let v2 = map_expression env v2 in
      R.Tuple [v1; v2]
    )
  | `Move_or_copy_exp (v1, v2) -> R.Case ("Move_or_copy_exp",
      let v1 =
        (match v1 with
        | `Move tok -> R.Case ("Move",
            (* "move" *) token env tok
          )
        | `Copy tok -> R.Case ("Copy",
            (* "copy" *) token env tok
          )
        )
      in
      let v2 =
        (* pattern (`)?[a-zA-Z_][0-9a-zA-Z_]*(`)? *) token env v2
      in
      R.Tuple [v1; v2]
    )
  | `Exp_term x -> R.Case ("Exp_term",
      map_expression_term env x
    )
  )

let map_struct_item (env : env) (x : CST.struct_item) =
  (match x with
  | `Native_struct_defi (v1, v2, v3, v4) -> R.Case ("Native_struct_defi",
      let v1 =
        (match v1 with
        | Some tok -> R.Option (Some (
            (* "public" *) token env tok
          ))
        | None -> R.Option None)
      in
      let v2 = (* "native" *) token env v2 in
      let v3 = map_struct_signature env v3 in
      let v4 = (* ";" *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Struct_defi (v1, v2, v3, v4) -> R.Case ("Struct_defi",
      let v1 =
        (match v1 with
        | Some tok -> R.Option (Some (
            (* "public" *) token env tok
          ))
        | None -> R.Option None)
      in
      let v2 = map_struct_signature env v2 in
      let v3 = map_datatype_fields env v3 in
      let v4 =
        (match v4 with
        | Some x -> R.Option (Some (
            map_postfix_ability_decls env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3; v4]
    )
  )

let map_enum_variants (env : env) ((v1, v2, v3, v4) : CST.enum_variants) =
  let v1 = (* "{" *) token env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = map_variant env v1 in
      let v2 = (* "," *) token env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_variant env x
      ))
    | None -> R.Option None)
  in
  let v4 = (* "}" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

let map_constant (env : env) ((v1, v2, v3, v4, v5, v6, v7) : CST.constant) =
  let v1 = (* "const" *) token env v1 in
  let v2 =
    (* pattern (`)?[a-zA-Z_][0-9a-zA-Z_]*(`)? *) token env v2
  in
  let v3 = (* ":" *) token env v3 in
  let v4 = map_type_ env v4 in
  let v5 = (* "=" *) token env v5 in
  let v6 = map_expression env v6 in
  let v7 = (* ";" *) token env v7 in
  R.Tuple [v1; v2; v3; v4; v5; v6; v7]

let map_function_item (env : env) (x : CST.function_item) =
  (match x with
  | `Native_func_defi (v1, v2) -> R.Case ("Native_func_defi",
      let v1 = map_function_signature env v1 in
      let v2 = (* ";" *) token env v2 in
      R.Tuple [v1; v2]
    )
  | `Macro_func_defi (v1, v2, v3, v4) -> R.Case ("Macro_func_defi",
      let v1 =
        (match v1 with
        | Some x -> R.Option (Some (
            map_modifier env x
          ))
        | None -> R.Option None)
      in
      let v2 = (* "macro" *) token env v2 in
      let v3 = map_macro_signature env v3 in
      let v4 = map_block env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Func_defi (v1, v2) -> R.Case ("Func_defi",
      let v1 = map_function_signature env v1 in
      let v2 = map_block env v2 in
      R.Tuple [v1; v2]
    )
  )

let map_enum_item (env : env) (x : CST.enum_item) =
  (match x with
  | `Enum_defi (v1, v2, v3, v4) -> R.Case ("Enum_defi",
      let v1 =
        (match v1 with
        | Some tok -> R.Option (Some (
            (* "public" *) token env tok
          ))
        | None -> R.Option None)
      in
      let v2 = map_enum_signature env v2 in
      let v3 = map_enum_variants env v3 in
      let v4 =
        (match v4 with
        | Some x -> R.Option (Some (
            map_postfix_ability_decls env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3; v4]
    )
  )

let map_module_body (env : env) ((v1, v2, v3) : CST.module_body) =
  let v1 =
    (match v1 with
    | `SEMI tok -> R.Case ("SEMI",
        (* ";" *) token env tok
      )
    | `LCURL tok -> R.Case ("LCURL",
        (* "{" *) token env tok
      )
    )
  in
  let v2 =
    R.List (List.map (fun x ->
      (match x with
      | `Use_decl x -> R.Case ("Use_decl",
          map_use_declaration env x
        )
      | `Friend_decl x -> R.Case ("Friend_decl",
          map_friend_declaration env x
        )
      | `Cst x -> R.Case ("Cst",
          map_constant env x
        )
      | `Func_item x -> R.Case ("Func_item",
          map_function_item env x
        )
      | `Struct_item x -> R.Case ("Struct_item",
          map_struct_item env x
        )
      | `Enum_item x -> R.Case ("Enum_item",
          map_enum_item env x
        )
      | `Spec_blk x -> R.Case ("Spec_blk",
          map_spec_block env x
        )
      )
    ) v2)
  in
  let v3 =
    (match v3 with
    | Some tok -> R.Option (Some (
        (* "}" *) token env tok
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3]

let map_module_definition (env : env) ((v1, v2, v3) : CST.module_definition) =
  let v1 = (* "module" *) token env v1 in
  let v2 = map_module_identity env v2 in
  let v3 = map_module_body env v3 in
  R.Tuple [v1; v2; v3]

let map_source_file (env : env) (xs : CST.source_file) =
  R.List (List.map (map_module_definition env) xs)

let map_whitespace (env : env) (tok : CST.whitespace) =
  (* pattern \s *) token env tok

let map_empty_line (env : env) (tok : CST.empty_line) =
  (* empty_line *) token env tok

let map_block_comment (env : env) (tok : CST.block_comment) =
  (* block_comment *) token env tok

let map_line_comment (env : env) (tok : CST.line_comment) =
  (* line_comment *) token env tok

let map_annotation (env : env) ((v1, v2, v3, v4, v5) : CST.annotation) =
  let v1 = (* "#[" *) token env v1 in
  let v2 = map_annotation_item env v2 in
  let v3 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_annotation_item env v2 in
      R.Tuple [v1; v2]
    ) v3)
  in
  let v4 =
    (match v4 with
    | Some tok -> R.Option (Some (
        (* "," *) token env tok
      ))
    | None -> R.Option None)
  in
  let v5 = (* "]" *) token env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

let dump_tree root =
  map_source_file () root
  |> Tree_sitter_run.Raw_tree.to_channel stdout

let map_extra (env : env) (x : CST.extra) =
  match x with
  | `Whitespace (_loc, x) -> ("whitespace", "whitespace", map_whitespace env x)
  | `Line_comment (_loc, x) -> ("line_comment", "line_comment", map_line_comment env x)
  | `Block_comment (_loc, x) -> ("block_comment", "block_comment", map_block_comment env x)
  | `Empty_line (_loc, x) -> ("empty_line", "empty_line", map_empty_line env x)
  | `Annotation (_loc, x) -> ("annotation", "annotation", map_annotation env x)

let dump_extras (extras : CST.extras) =
  List.iter (fun extra ->
    let ts_rule_name, ocaml_type_name, raw_tree = map_extra () extra in
    let details =
      if ocaml_type_name <> ts_rule_name then
        Printf.sprintf " (OCaml type '%s')" ocaml_type_name
      else
        ""
    in
    Printf.printf "%s%s:\n" ts_rule_name details;
    Tree_sitter_run.Raw_tree.to_channel stdout raw_tree
  ) extras
