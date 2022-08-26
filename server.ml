open Httpaf
open Lwt.Infix
module Arg = Caml.Arg
open Httpaf_lwt_unix

let error_handler ?request:_ _error _start_response = failwith "error"

let echo_post { Gluten.reqd; _ } =
  match Reqd.request reqd with
  | { Request.meth = `POST; headers; target; _ } ->
      Format.printf "POST %s\n%!" target;
      let response =
        let content_type =
          match Headers.get headers "content-type" with
          | None -> "application/octet-stream"
          | Some x -> x
        in
        Response.create
          ~headers:(Headers.of_list [ ("content-type", content_type) ])
          `OK
      in
      let request_body = Reqd.request_body reqd in
      let response_body = Reqd.respond_with_streaming reqd response in
      let rec on_read buffer ~off ~len =
        Body.Writer.write_bigstring response_body buffer ~off ~len;
        Body.Reader.schedule_read request_body ~on_eof ~on_read
      and on_eof () = Body.Writer.close response_body in
      Body.Reader.schedule_read (Reqd.request_body reqd) ~on_eof ~on_read
  | _ ->
      let headers = Headers.of_list [ ("connection", "close") ] in
      Reqd.respond_with_string reqd
        (Response.create ~headers `Method_not_allowed)
        ""

let error_handler (_ : Unix.sockaddr) = error_handler
let request_handler (_ : Unix.sockaddr) = echo_post

let main port =
  let listen_address = Unix.(ADDR_INET (inet_addr_loopback, port)) in
  Lwt.async (fun () ->
      Lwt_io.establish_server_with_client_socket listen_address
        (Server.create_connection_handler ~request_handler ~error_handler)
      >|= fun _server ->
      Format.printf "Listening on port %i and echoing POST requests.\n" port);
  let forever, _ = Lwt.wait () in
  Lwt_main.run forever

let () =
  let port = 8080 in
  main port
