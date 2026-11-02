(* [@@deriving message]: a message type's [Causet.Message.t] and its printer,
   from the type, as the Rust derives make them.

   {[
     type cart_action = Add of { item : string; price : int } | Checkout
     [@@deriving message ~name:"cart.action"]
     (* val pp_cart_action : Format.formatter -> cart_action -> unit
        val cart_action_message : cart_action Causet.Message.t
        kinds "cart.add" and "cart.checkout" *)
   ]}

   Type options: [~name] (required: the type's name), [~prefix] (the kinds'
   prefix; default [name] without its last [.] segment), [~kind] (a record's
   kind; default [name]), [~level] (the level logging records a value at:
   [Trace] to [Error]; default [Debug]), [~fields] (a record's logged
   fields, as [[@fields]] below), and at most one of:

   - [~pp:printer], a printer written by hand, of type
     [Format.formatter -> <type> -> unit]. None is derived: the deriver
     binds this one as [pp_<type>], so the message prints with it and so do
     the printers of the types that hold this one. It is evaluated after the
     type, so it may name the type's constructors and fields. One type per
     [[@@deriving]].
   - [~no_pp], no printer at all: the deriver makes no [pp_<type>], and the
     message prints every value as [_]. For a type the deriver cannot print
     (an abstract or extensible one) or whose values nothing needs to
     print. A type that holds this one then needs a [pp_<type>] of its own;
     with [~pp:Causet.Debug.opaque] in place of [~no_pp], it gets one that
     prints [_].

   [@@deriving debug] makes the printer alone, for the types a message
   holds.

   Constructor attributes: [[@kind "..."]] (its kind), [[@debug_name "..."]]
   (the name its printer gives it, for a constructor named apart from the
   Rust one), [[@nested]] (a
   constructor of one argument holding a message, whose kind, level and
   fields it takes), [[@level Warn]] and [[@fields a, n = b, display c]] (the
   fields of its inline record logging records, by their printer unless
   [display]). A [[@nested]] constructor takes no [[@kind]], [[@level]] or
   [[@fields]]: they are its inner message's. An attribute [[@causet.x]] the
   deriver does not know is an error.

   A constructor's kind is the prefix and its name in snake case, as Rust's:
   [Checked_out] and [CheckedOut] both give [checked_out]. The printer is
   Rust's [Debug] form ([Checked_out] prints as [CheckedOut]), so payloads
   print as the Rust port's do; a field of type [foo] is printed by
   [pp_foo], of [M.t] by [M.pp], of [M.foo] by [M.pp_foo]. A type named [t]
   gives [pp] and [message]; one named [effect_] gives [pp_effect] and
   [effect_message], and a field [to_] prints as [to].

   The generated code's own variables start with [__causet_], and a field
   is bound as [__causet_field_<name>], so no field can hide the
   formatter, a printer or another field. *)

open Ppxlib
module B = Ast_builder.Default

let snake name =
  let b = Buffer.create (String.length name + 4) in
  String.iteri
    (fun i c ->
      if Char.uppercase_ascii c = c && Char.lowercase_ascii c <> c then (
        if i > 0 && name.[i - 1] <> '_' then Buffer.add_char b '_';
        Buffer.add_char b (Char.lowercase_ascii c))
      else Buffer.add_char b c)
    name;
  Buffer.contents b

(* [checked_out] or [Checked_out] as Rust names it: [CheckedOut]. *)
let camel name =
  let b = Buffer.create (String.length name) in
  let up = ref true in
  String.iter
    (fun c ->
      if c = '_' then up := true
      else (
        Buffer.add_char b (if !up then Char.uppercase_ascii c else c);
        up := false))
    name;
  Buffer.contents b

(* A trailing [_] escapes a keyword ([effect_]) and is dropped. *)
let base n =
  if String.ends_with ~suffix:"_" n then String.sub n 0 (String.length n - 1) else n

let printer_name = function "t" -> "pp" | n -> "pp_" ^ base n
let message_name = function "t" -> "message" | n -> base n ^ "_message"

(* The generated code's variables. A field [x] is bound as
   [__causet_field_x], which none of the others is, so no field or
   constructor argument can capture the formatter, the value, a printer or
   another field. *)
let fmt = "__causet_fmt"
let value = "__causet_value"
let inner = "__causet_inner"
let arg i = Printf.sprintf "__causet_arg%d" i
let field_var name = "__causet_field_" ^ name

(* ---------------------------------------------------------------------- *)
(* Attributes *)

let kind_attr =
  Attribute.declare "causet.kind" Attribute.Context.constructor_declaration
    Ast_pattern.(single_expr_payload (estring __))
    (fun s -> s)

let nested_attr =
  Attribute.declare "causet.nested" Attribute.Context.constructor_declaration
    Ast_pattern.(pstr nil)
    ()

let level_attr =
  Attribute.declare "causet.level" Attribute.Context.constructor_declaration
    Ast_pattern.(single_expr_payload (pexp_construct (lident __') none))
    (fun s -> s)

let debug_name_attr =
  Attribute.declare "causet.debug_name" Attribute.Context.constructor_declaration
    Ast_pattern.(single_expr_payload (estring __))
    (fun s -> s)

let fields_attr =
  Attribute.declare "causet.fields" Attribute.Context.constructor_declaration
    Ast_pattern.(single_expr_payload __)
    (fun e -> e)

(* The constructor attributes, by the names they are written with. *)
let known_attrs = [ "kind"; "nested"; "level"; "fields"; "debug_name" ]

(* Where [[@name]] is written on [cd], to report an error on it. *)
let attr_loc (cd : constructor_declaration) name =
  match
    List.find_opt
      (fun a -> a.attr_name.txt = name || a.attr_name.txt = "causet." ^ name)
      cd.pcd_attributes
  with
  | Some a -> a.attr_loc
  | None -> cd.pcd_loc

(* Rust refuses an option it does not know; so does the deriver, for an
   attribute in its own namespace. *)
let check_attrs (cd : constructor_declaration) =
  List.iter
    (fun a ->
      let n = a.attr_name.txt in
      if
        String.starts_with ~prefix:"causet." n
        && not (List.mem (String.sub n 7 (String.length n - 7)) known_attrs)
      then
        Location.raise_errorf ~loc:a.attr_loc
          "[@%s]: expected [@kind], [@nested], [@level], [@fields] or [@debug_name]" n)
    cd.pcd_attributes

let levels = [ "Trace"; "Debug"; "Info"; "Warn"; "Error" ]

let level_expr ~loc l =
  if not (List.mem l levels) then
    Location.raise_errorf ~loc "expected Trace, Debug, Info, Warn or Error";
  B.pexp_construct ~loc { txt = Ldot (Ldot (Lident "Causet", "Message"), l); loc } None

(* A logged field: its logged name, the record field (and where it is
   named), by [display] or not. *)
type field = { logged : string; field : string; at : location; display : bool }

let parse_fields e =
  let one e =
    match e.pexp_desc with
    | Pexp_ident { txt = Lident f; loc } ->
        { logged = f; field = f; at = loc; display = false }
    | Pexp_apply
        ( { pexp_desc = Pexp_ident { txt = Lident (("display" | "debug") as how); _ }; _ },
          [ (Nolabel, { pexp_desc = Pexp_ident { txt = Lident f; loc }; _ }) ] ) ->
        { logged = f; field = f; at = loc; display = how = "display" }
    | Pexp_apply
        ( { pexp_desc = Pexp_ident { txt = Lident "="; _ }; _ },
          [
            (Nolabel, { pexp_desc = Pexp_ident { txt = Lident name; _ }; _ });
            (Nolabel, value);
          ] ) -> (
        match value.pexp_desc with
        | Pexp_ident { txt = Lident f; loc } ->
            { logged = name; field = f; at = loc; display = false }
        | Pexp_apply
            ( {
                pexp_desc = Pexp_ident { txt = Lident (("display" | "debug") as how); _ };
                _;
              },
              [ (Nolabel, { pexp_desc = Pexp_ident { txt = Lident f; loc }; _ }) ] ) ->
            { logged = name; field = f; at = loc; display = how = "display" }
        | _ ->
            Location.raise_errorf ~loc:value.pexp_loc
              "expected a field, debug field or display field")
    | _ ->
        Location.raise_errorf ~loc:e.pexp_loc
          "expected a field, name = field, or display field"
  in
  match e.pexp_desc with Pexp_tuple es -> List.map one es | _ -> [ one e ]

(* ---------------------------------------------------------------------- *)
(* Printers *)

let debug ~loc name = B.evar ~loc ("Causet.Debug." ^ name)
let evar ~loc n = B.evar ~loc n
let pvar ~loc n = B.pvar ~loc n

(* [fun __causet_fmt -> pp __causet_fmt v]: an item of [Debug.tuple] or
   [Debug.record]. *)
let item ~loc pp v = [%expr fun [%p pvar ~loc fmt] -> [%e pp] [%e evar ~loc fmt] [%e v]]

let rec printer ~loc (ty : core_type) =
  match ty.ptyp_desc with
  | Ptyp_constr
      ( {
          txt =
            Lident
              (( "int" | "int32" | "int64" | "string" | "bytes" | "bool" | "char" | "unit"
               | "float" ) as base);
          _;
        },
        [] ) ->
      debug ~loc base
  | Ptyp_constr ({ txt = Lident (("list" | "array" | "option") as c); _ }, [ t ]) ->
      [%expr [%e debug ~loc c] [%e printer ~loc t]]
  | Ptyp_constr ({ txt = Lident n; _ }, args) ->
      apply ~loc (B.evar ~loc (printer_name n)) args
  | Ptyp_constr ({ txt = Ldot (m, n); _ }, args) ->
      apply ~loc (B.pexp_ident ~loc { txt = Ldot (m, printer_name n); loc }) args
  | Ptyp_tuple ts ->
      let names = List.mapi (fun i _ -> arg i) ts in
      let pat = B.ppat_tuple ~loc (List.map (pvar ~loc) names) in
      let items =
        List.map2 (fun n t -> item ~loc (printer ~loc t) (evar ~loc n)) names ts
      in
      [%expr
        fun [%p pvar ~loc fmt] [%p pat] ->
          Causet.Debug.tuple [%e evar ~loc fmt] "" [%e B.elist ~loc items]]
  | _ -> debug ~loc "opaque"

and apply ~loc f args =
  match args with [] -> f | _ -> B.eapply ~loc f (List.map (printer ~loc) args)

(* A field's display form, Rust's [Display]: base types as [to_string]
   writes them, a string unquoted; [M.t] by [M.to_string], [foo] by
   [foo_to_string]. *)
let display ~loc (ty : core_type) v =
  match ty.ptyp_desc with
  | Ptyp_constr ({ txt = Lident "string"; _ }, []) -> v
  | Ptyp_constr ({ txt = Lident "int"; _ }, []) -> [%expr Stdlib.string_of_int [%e v]]
  | Ptyp_constr ({ txt = Lident "int32"; _ }, []) -> [%expr Stdlib.Int32.to_string [%e v]]
  | Ptyp_constr ({ txt = Lident "int64"; _ }, []) -> [%expr Stdlib.Int64.to_string [%e v]]
  | Ptyp_constr ({ txt = Lident "bool"; _ }, []) -> [%expr Stdlib.string_of_bool [%e v]]
  | Ptyp_constr ({ txt = Lident "char"; _ }, []) -> [%expr Stdlib.String.make 1 [%e v]]
  | Ptyp_constr ({ txt = Lident "float"; _ }, []) ->
      [%expr Causet.Debug.to_string Causet.Debug.float [%e v]]
  | Ptyp_constr ({ txt = Ldot (m, "t"); _ }, []) ->
      B.eapply ~loc (B.pexp_ident ~loc { txt = Ldot (m, "to_string"); loc }) [ v ]
  | Ptyp_constr ({ txt = Ldot (m, n); _ }, []) ->
      B.eapply ~loc (B.pexp_ident ~loc { txt = Ldot (m, n ^ "_to_string"); loc }) [ v ]
  | Ptyp_constr ({ txt = Lident n; _ }, []) ->
      B.eapply ~loc (B.evar ~loc (n ^ "_to_string")) [ v ]
  | _ -> Location.raise_errorf ~loc "a displayed field is a base type, M.t or foo"

(* The value a [Some]-nested constructor's message is named by. *)
let message_of (ty : core_type) =
  let loc = ty.ptyp_loc in
  match ty.ptyp_desc with
  | Ptyp_constr ({ txt = Lident n; _ }, []) -> B.evar ~loc (message_name n)
  | Ptyp_constr ({ txt = Ldot (m, n); _ }, []) ->
      B.pexp_ident ~loc { txt = Ldot (m, message_name n); loc }
  | _ ->
      Location.raise_errorf ~loc "a nested constructor holds a message type: foo or M.t"

let record_items ~loc labels =
  B.elist ~loc
    (List.map
       (fun (ld : label_declaration) ->
         let name = ld.pld_name.txt in
         [%expr
           [%e B.estring ~loc (base name)],
           [%e item ~loc (printer ~loc ld.pld_type) (evar ~loc (field_var name))]])
       labels)

(* [{ a = __causet_field_a; ... }]: every field, or with [~closed:Open]
   the ones named. *)
let record_pattern ?(closed = Closed) ~loc names =
  B.ppat_record ~loc
    (List.map (fun n -> ({ txt = Lident n; loc }, pvar ~loc (field_var n))) names)
    closed

let label_names labels = List.map (fun (ld : label_declaration) -> ld.pld_name.txt) labels

let constructor_pattern ~loc (cd : constructor_declaration) arg =
  B.ppat_construct ~loc { txt = Lident cd.pcd_name.txt; loc } arg

(* [fun (__causet_value : ty) -> match __causet_value with cases]: annotated,
   so a constructor or field another type shares is the one of [ty]. *)
let match_value ~loc ty cases =
  let cases =
    match cases with
    | [] -> [ B.case ~lhs:(B.ppat_any ~loc) ~guard:None ~rhs:(B.pexp_unreachable ~loc) ]
    | _ -> cases
  in
  [%expr
    fun ([%p pvar ~loc value] : [%t ty]) -> [%e B.pexp_match ~loc (evar ~loc value) cases]]

let type_of ~loc (td : type_declaration) =
  B.ptyp_constr ~loc { txt = Lident td.ptype_name.txt; loc } []

let pp_case ~loc (cd : constructor_declaration) =
  let f = evar ~loc fmt in
  let shown =
    B.estring ~loc
      (match Attribute.get debug_name_attr cd with
      | Some n -> n
      | None -> camel cd.pcd_name.txt)
  in
  match cd.pcd_args with
  | Pcstr_tuple [] ->
      B.case
        ~lhs:(constructor_pattern ~loc cd None)
        ~guard:None
        ~rhs:[%expr Stdlib.Format.pp_print_string [%e f] [%e shown]]
  | Pcstr_tuple tys ->
      let names = List.mapi (fun i _ -> arg i) tys in
      let pat =
        match names with
        | [ n ] -> pvar ~loc n
        | _ -> B.ppat_tuple ~loc (List.map (pvar ~loc) names)
      in
      let items =
        List.map2 (fun n t -> item ~loc (printer ~loc t) (evar ~loc n)) names tys
      in
      B.case
        ~lhs:(constructor_pattern ~loc cd (Some pat))
        ~guard:None
        ~rhs:[%expr Causet.Debug.tuple [%e f] [%e shown] [%e B.elist ~loc items]]
  | Pcstr_record labels ->
      B.case
        ~lhs:
          (constructor_pattern ~loc cd (Some (record_pattern ~loc (label_names labels))))
        ~guard:None
        ~rhs:[%expr Causet.Debug.record [%e f] [%e shown] [%e record_items ~loc labels]]

let printer_binding ~loc (td : type_declaration) =
  let name = td.ptype_name.txt in
  let ty = type_of ~loc td in
  let f = evar ~loc fmt in
  let body =
    match td.ptype_kind with
    | Ptype_variant cds -> match_value ~loc ty (List.map (pp_case ~loc) cds)
    | Ptype_record labels ->
        match_value ~loc ty
          [
            B.case
              ~lhs:(record_pattern ~loc (label_names labels))
              ~guard:None
              ~rhs:
                [%expr
                  Causet.Debug.record [%e f]
                    [%e B.estring ~loc (camel name)]
                    [%e record_items ~loc labels]];
          ]
    | Ptype_abstract -> (
        match td.ptype_manifest with
        | Some manifest ->
            [%expr
              fun ([%p pvar ~loc value] : [%t ty]) ->
                [%e printer ~loc manifest] [%e f] [%e evar ~loc value]]
        | None ->
            Location.raise_errorf ~loc
              "an abstract type has no derived printer: write one with ~pp, or use ~no_pp"
        )
    | Ptype_open ->
        Location.raise_errorf ~loc
          "an extensible type has no derived printer: write one with ~pp, or use ~no_pp"
  in
  B.value_binding ~loc
    ~pat:(pvar ~loc (printer_name name))
    ~expr:[%expr fun ([%p pvar ~loc fmt] : Stdlib.Format.formatter) -> [%e body]]

(* ---------------------------------------------------------------------- *)
(* Messages *)

(* A record's logged fields [e]: the pattern binding them and the list
   logging records. *)
let logged_fields ~loc labels e =
  let fields = parse_fields e in
  let typed f =
    match
      List.find_opt (fun (ld : label_declaration) -> ld.pld_name.txt = f.field) labels
    with
    | Some ld -> ld.pld_type
    | None -> Location.raise_errorf ~loc:f.at "no field %s" f.field
  in
  let render f =
    let t = typed f in
    let v = evar ~loc (field_var f.field) in
    if f.display then display ~loc t v
    else [%expr Causet.Debug.to_string [%e printer ~loc t] [%e v]]
  in
  let bound = List.sort_uniq compare (List.map (fun f -> f.field) fields) in
  ( record_pattern ~closed:Open ~loc bound,
    B.elist ~loc
      (List.map (fun f -> [%expr [%e B.estring ~loc f.logged], [%e render f]]) fields) )

(* A [[@nested]] constructor's kind, level and fields are its inner
   message's: Rust refuses [kind] and [log] on one, and so does the
   deriver. *)
let check_nested (cd : constructor_declaration) =
  let refuse attr name why =
    match Attribute.get attr cd with
    | None -> ()
    | Some _ -> Location.raise_errorf ~loc:(attr_loc cd name) "%s" why
  in
  refuse kind_attr "kind" "a constructor is either [@nested] or has a [@kind], not both";
  refuse level_attr "level"
    "a [@nested] constructor logs as its inner message: no [@level]";
  refuse fields_attr "fields"
    "a [@nested] constructor logs as its inner message: no [@fields]"

let message_binding ~loc ~name ~prefix ~kind ~level ~fields ~no_pp (td : type_declaration)
    =
  let ty = td.ptype_name.txt in
  let ty_expr = type_of ~loc td in
  let type_level = Option.map (fun l -> level_expr ~loc:l.loc l.txt) level in
  let default_level = Option.value type_level ~default:(level_expr ~loc "Debug") in
  let prefix =
    match prefix with
    | Some p -> p
    | None -> (
        match String.rindex_opt name '.' with
        | Some i -> String.sub name 0 i
        | None -> name)
  in
  let kind_fn, level_fn, fields_fn =
    match td.ptype_kind with
    | Ptype_variant _ when kind <> None ->
        let k = Option.get kind in
        Location.raise_errorf ~loc:k.loc
          "~kind names a record's kind: give a constructor [@kind]"
    | Ptype_variant _ when fields <> None ->
        let e = Option.get fields in
        Location.raise_errorf ~loc:e.pexp_loc
          "~fields names a record's fields: give a constructor [@fields]"
    | Ptype_variant cds ->
        let any_level = ref (type_level <> None) and any_fields = ref false in
        let cases =
          List.map
            (fun (cd : constructor_declaration) ->
              check_attrs cd;
              let nested = Attribute.get nested_attr cd <> None in
              let wild =
                match cd.pcd_args with
                | Pcstr_tuple [] -> None
                | _ -> Some (B.ppat_any ~loc)
              in
              if nested then (
                check_nested cd;
                let inner_ty =
                  match cd.pcd_args with
                  | Pcstr_tuple [ t ] -> t
                  | _ ->
                      Location.raise_errorf ~loc:cd.pcd_loc
                        "a nested constructor holds exactly one message"
                in
                let m = message_of inner_ty in
                any_level := true;
                any_fields := true;
                let pat = constructor_pattern ~loc cd (Some (pvar ~loc inner)) in
                let v = evar ~loc inner in
                ( B.case ~lhs:pat ~guard:None
                    ~rhs:[%expr [%e m].Causet.Message.kind [%e v]],
                  B.case ~lhs:pat ~guard:None
                    ~rhs:[%expr [%e m].Causet.Message.level [%e v]],
                  B.case ~lhs:pat ~guard:None
                    ~rhs:[%expr [%e m].Causet.Message.fields [%e v]] ))
              else
                let kind =
                  match Attribute.get kind_attr cd with
                  | Some k -> k
                  | None -> prefix ^ "." ^ snake cd.pcd_name.txt
                in
                let level =
                  match Attribute.get level_attr cd with
                  | Some l ->
                      any_level := true;
                      level_expr ~loc:l.loc l.txt
                  | None -> default_level
                in
                let fields_case =
                  match Attribute.get fields_attr cd with
                  | None ->
                      B.case
                        ~lhs:(constructor_pattern ~loc cd wild)
                        ~guard:None ~rhs:[%expr []]
                  | Some e -> (
                      any_fields := true;
                      match cd.pcd_args with
                      | Pcstr_record labels ->
                          let pat, rhs = logged_fields ~loc labels e in
                          B.case
                            ~lhs:(constructor_pattern ~loc cd (Some pat))
                            ~guard:None ~rhs
                      | _ ->
                          Location.raise_errorf ~loc:e.pexp_loc
                            "logged fields name an inline record's fields")
                in
                ( B.case
                    ~lhs:(constructor_pattern ~loc cd wild)
                    ~guard:None ~rhs:(B.estring ~loc kind),
                  B.case ~lhs:(constructor_pattern ~loc cd wild) ~guard:None ~rhs:level,
                  fields_case ))
            cds
        in
        let ks, ls, fs =
          List.fold_right
            (fun (k, l, f) (ks, ls, fs) -> (k :: ks, l :: ls, f :: fs))
            cases ([], [], [])
        in
        ( match_value ~loc ty_expr ks,
          (if !any_level && cds <> [] then Some (match_value ~loc ty_expr ls) else None),
          if !any_fields && cds <> [] then Some (match_value ~loc ty_expr fs) else None )
    | _ ->
        let kind = match kind with Some k -> k.txt | None -> name in
        let fields_fn =
          match (fields, td.ptype_kind) with
          | None, _ -> None
          | Some e, Ptype_record labels ->
              let pat, rhs = logged_fields ~loc labels e in
              Some (match_value ~loc ty_expr [ B.case ~lhs:pat ~guard:None ~rhs ])
          | Some e, _ ->
              Location.raise_errorf ~loc:e.pexp_loc "logged fields name a record's fields"
        in
        ( [%expr fun (_ : [%t ty_expr]) -> [%e B.estring ~loc kind]],
          Option.map (fun l -> [%expr fun (_ : [%t ty_expr]) -> [%e l]]) type_level,
          fields_fn )
  in
  let args =
    [ (Labelled "name", B.estring ~loc name); (Labelled "kind", kind_fn) ]
    @ (match level_fn with Some l -> [ (Labelled "level", l) ] | None -> [])
    @ (match fields_fn with Some f -> [ (Labelled "fields", f) ] | None -> [])
    @ (if no_pp then [] else [ (Labelled "pp", evar ~loc (printer_name ty)) ])
    @ [ (Nolabel, [%expr ()]) ]
  in
  let make = B.pexp_apply ~loc [%expr Causet.Message.make] args in
  B.value_binding ~loc
    ~pat:
      (B.ppat_constraint ~loc
         (pvar ~loc (message_name ty))
         [%type: [%t ty_expr] Causet.Message.t])
    ~expr:make

let printers ~loc tds =
  (* Scoped to the printers: a printer needn't call itself. *)
  let rec_value = B.pstr_value ~loc Recursive (List.map (printer_binding ~loc) tds) in
  [
    B.pstr_include ~loc
      (B.include_infos ~loc
         (B.pmod_structure ~loc [ [%stri [@@@ocaml.warning "-39"]]; rec_value ]));
  ]

(* [~pp]: the printer written by hand, bound as the type's [pp_<type>]. *)
let given_printer ~loc (td : type_declaration) pp =
  B.pstr_value ~loc Nonrecursive
    [
      B.value_binding ~loc
        ~pat:
          (B.ppat_constraint ~loc
             (pvar ~loc (printer_name td.ptype_name.txt))
             [%type: Stdlib.Format.formatter -> [%t type_of ~loc td] -> unit])
        ~expr:pp;
    ]

let printer_sig ~loc (td : type_declaration) =
  B.psig_value ~loc
    (B.value_description ~loc
       ~name:{ txt = printer_name td.ptype_name.txt; loc }
       ~type_:[%type: Stdlib.Format.formatter -> [%t type_of ~loc td] -> unit] ~prim:[])

let generate_impl ~ctxt (_rec_flag, tds) name prefix kind level fields pp no_pp =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  let name =
    match name with
    | Some n -> n
    | None ->
        Location.raise_errorf ~loc "[@@deriving message] needs ~name, the type's name"
  in
  let printers =
    match (pp, no_pp, tds) with
    | None, false, _ -> printers ~loc tds
    | None, true, _ -> []
    | Some pp, true, _ ->
        Location.raise_errorf ~loc:pp.pexp_loc
          "~pp gives the printer and ~no_pp leaves it out: use one"
    | Some pp, false, [ td ] -> [ given_printer ~loc td pp ]
    | Some pp, false, _ ->
        Location.raise_errorf ~loc:pp.pexp_loc
          "~pp is one type's printer: derive these types apart"
  in
  printers
  @ List.map
      (fun td ->
        B.pstr_value ~loc Nonrecursive
          [ message_binding ~loc ~name ~prefix ~kind ~level ~fields ~no_pp td ])
      tds

let generate_intf ~ctxt (_rec_flag, tds) _name _prefix _kind _level _fields _pp no_pp =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  List.concat_map
    (fun (td : type_declaration) ->
      (if no_pp then [] else [ printer_sig ~loc td ])
      @ [
          B.psig_value ~loc
            (B.value_description ~loc
               ~name:{ txt = message_name td.ptype_name.txt; loc }
               ~type_:[%type: [%t type_of ~loc td] Causet.Message.t] ~prim:[]);
        ])
    tds

let args () =
  Deriving.Args.(
    empty
    +> arg "name" (estring __)
    +> arg "prefix" (estring __)
    +> arg "kind" (estring __')
    +> arg "level" (pexp_construct (lident __') none)
    +> arg "fields" __ +> arg "pp" __ +> flag "no_pp")

let () =
  ignore
    (Deriving.add "debug"
       ~str_type_decl:
         (Deriving.Generator.V2.make Deriving.Args.empty (fun ~ctxt (_, tds) ->
              printers ~loc:(Expansion_context.Deriver.derived_item_loc ctxt) tds))
       ~sig_type_decl:
         (Deriving.Generator.V2.make Deriving.Args.empty (fun ~ctxt (_, tds) ->
              List.map
                (printer_sig ~loc:(Expansion_context.Deriver.derived_item_loc ctxt))
                tds)));
  ignore
    (Deriving.add "message"
       ~str_type_decl:(Deriving.Generator.V2.make (args ()) generate_impl)
       ~sig_type_decl:(Deriving.Generator.V2.make (args ()) generate_intf))
