open Js_of_ocaml_toplevel

let append_string output cl s =
    let open Js_of_ocaml in
    let d = Dom_html.window##.document in
    let span = Dom_html.createDiv d in
    span##.classList##add (Js.string cl);
    Dom.appendChild span (d##createTextNode (Js.string s));
    Dom.appendChild output span
;;

let configure o chan attr default =
    let open Js_of_ocaml in
    try
        let v = o##getAttribute (Js.string attr) in
        match Js.Opt.to_option v with
        | None -> raise Not_found
        | Some id ->
            let dom = Dom_html.getElementById (Js.to_string id) in
            Sys_js.set_channel_flusher chan (append_string dom attr)
    with Not_found ->
        Sys_js.set_channel_flusher chan default
;;

let prepare_code content =
  let len = String.length content in
  if try content <> "" && content.[len - 1] <> ';' && content.[len - 2] <> ';'
     with _ -> true
  then content ^ ";;"
  else content
;;

let setup_toplevel() =
    let buffer = Buffer.create 100 in
    let formatter = Format.formatter_of_buffer buffer in
    JsooTop.initialize();
    Ast_mapper.register "js_of_ocaml" (fun _ -> Ppx_js.mapper);
    let open Js_of_ocaml in
    let dom = Dom_html.getElementById "stdout" in
    Sys_js.set_channel_flusher stdout (append_string dom "stdout");
    let dom = Dom_html.getElementById "stderr" in
    Sys_js.set_channel_flusher stderr (append_string dom "stderr");
    JsooTop.execute false formatter "let webgl_ctx : Js_of_ocaml.WebGL.renderingContext Js_of_ocaml.Js.t = Js_of_ocaml.Js.Unsafe.variable \"webgl_ctx\";;\nlet canvas : Js_of_ocaml.Dom_html.canvasElement Js_of_ocaml.Js.t = Js_of_ocaml.Js.Unsafe.coerce (Js_of_ocaml.Dom_html.getElementById_exn \"canvas\");;";
;;

let execute code =
    setup_toplevel();
    let code = Js_of_ocaml.Js.to_string code |> prepare_code in
    let buffer = Buffer.create 100 in
    let formatter = Format.formatter_of_buffer buffer in
    JsooTop.execute false formatter code;
    Js_of_ocaml.Js.string (Buffer.contents buffer)
;;

let () =
    Js_of_ocaml.Js.export
        "evaluator"
        (object%js
            val execute = execute
        end)
;;

