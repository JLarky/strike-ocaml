open Tyxml

module H = struct
  type t = {
    (* *)
    tag : string;
    props : (string * string) list;
    children : t list;
  }

  let h tag props children = { tag; props; children }
end

module Strike = struct
  (* let message = "Hello" *)
  let rec render_to_string H.{ tag; props; children } =
    Printf.sprintf "<%s%s>%s%s</%s>" tag
      ( props
      |> List.filter (fun (k, _) -> k <> "children")
      |> List.map (fun (k, v) -> Printf.sprintf "%s=\"%s\"" k v)
      |> String.concat " "
      |> fun s -> if s = "" then s else " " ^ s )
      (children |> List.map render_to_string |> String.concat "")
      (props |> List.assoc_opt "children" |> Option.value ~default:"")
      tag
end

module Framework = struct
  let h = H.h

  (* https://github.com/JLarky/strike/blob/main/pkg/framework/framework.go#L9 *)
  (* return []Component{
     	H("script", Props{"type": "importmap"}, []template.HTML{`
     		{
     			"imports": {
     				"strike_islands": "/static/app/islands.js",
     				"react": "https://esm.sh/react@0.0.0-experimental-9ba1bbd65-20230922",
     				"react-dom/client": "https://esm.sh/react-dom@0.0.0-experimental-9ba1bbd65-20230922/client",
     				"react-dom": "https://esm.sh/react-dom@0.0.0-experimental-9ba1bbd65-20230922",
     				"react/jsx-runtime": "https://esm.sh/react@0.0.0-experimental-9ba1bbd65-20230922/jsx-runtime",
     				"react-error-boundary": "https://esm.sh/react-error-boundary@4.0.11"
     			}
     		}`}),
     	H("link", Props{"rel": "modulepreload", "href": "/_strike/bootstrap.js"}),
     	H("link", Props{"rel": "modulepreload", "href": "https://esm.sh/v132/react-error-boundary@4.0.11/es2022/react-error-boundary.mjs"}),
     	H("link", Props{"rel": "modulepreload", "href": "https://esm.sh/react-error-boundary@4.0.11"}),
     	H("link", Props{"rel": "modulepreload", "href": "https://esm.sh/react@0.0.0-experimental-9ba1bbd65-20230922?dev"}),
     	H("link", Props{"rel": "modulepreload", "href": "https://esm.sh/react-dom@0.0.0-experimental-9ba1bbd65-20230922"}),
     	H("link", Props{"rel": "modulepreload", "href": "https://esm.sh/react-dom@0.0.0-experimental-9ba1bbd65-20230922/client"}),
     	H("script", Props{"async": "async", "type": "module", "src": "/_strike/bootstrap.js"}),
     } *)

  let bootstrap =
    [
      h "script"
        [
          ("type", "importmap");
          ( "children",
            {|{
  "imports": {
    "strike_islands": "/static/app/islands.js",
    "react": "https://esm.sh/react@0.0.0-experimental-9ba1bbd65-20230922",
    "react-dom/client": "https://esm.sh/react-dom@0.0.0-experimental-9ba1bbd65-20230922/client",
    "react-dom": "https://esm.sh/react-dom@0.0.0-experimental-9ba1bbd65-20230922",
    "react/jsx-runtime": "https://esm.sh/react@0.0.0-experimental-9ba1bbd65-20230922/jsx-runtime",
    "react-error-boundary": "https://esm.sh/react-error-boundary@4.0.11"
  }
}|}
          );
        ]
        [];
      h "link"
        [ ("rel", "modulepreload"); ("href", "/_strike/bootstrap.js") ]
        [];
      h "script"
        [
          ("async", "async");
          ("type", "module");
          ("src", "/_strike/bootstrap.js");
        ]
        [];
    ]
end

module App = struct
  let h = H.h

  let head =
    [
      h "meta" [ ("charset", "utf-8") ] [];
      h "meta"
        [
          ("name", "viewport");
          ("content", "width=device-width, initial-scale=1.0");
        ]
        [];
      h "title" [] [ h "text" [ ("children", "Strike") ] [] ];
      h "meta"
        [
          ("name", "description");
          ("content", "Strike is OCaml React Server Components framework");
        ]
        [];
    ]

  let page =
    h "html"
      [ ("lang", "en") ]
      [
        h "head" [] (List.concat [ head; Framework.bootstrap ]);
        h "body" [] [ h "h1" [ ("children", "Hello") ] [] ];
      ]
end

let run () =
  Dream.run ~interface:"0.0.0.0"
  @@ Dream.logger
  @@ Dream.router
       [
         Dream.get "/static/app/**" @@ Dream.static "src/assets";
         Dream.get "/_strike/**" @@ Dream.static "src/strike_http/assets";
         Dream.get "/" (fun _ ->
             let mypage =
               Tyxml.Html.(div ~a:[ a_id "app" ] [ h1 [ txt "Hello" ] ])
             in
             let s = Format.asprintf "%a" (Html.pp_elt ()) mypage in
             let str = Strike.render_to_string App.page in
             let json =
               Printf.sprintf
                 {|<script>self.__rsc=self.__rsc||[];__rsc.push('%s')</script>|}
                 s
             in

             let response = Dream.response (str ^ json) in
             Dream.add_header response "Content-Type" Dream.text_html;
             Lwt.return response);
       ]
