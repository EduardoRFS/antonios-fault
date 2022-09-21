let ( let* ) = Lwt.bind
let listen_address = Unix.(ADDR_INET (inet_addr_loopback, 8000))

let server () =
  Lwt_io.establish_server_with_client_socket listen_address
    (Piaf.Server.create ?config:None ?error_handler:None
       (fun { ctx = _; request } ->
         let body = request.Piaf.Request.body in
         let* raw_content = Piaf.Body.to_string body in
         (match raw_content with
         | Ok raw_content ->
             Format.eprintf "received: %d\n%!" (String.length raw_content)
         | Error _error -> assert false);
         let response = Piaf.Response.of_string ~body:"OK" `OK in
         Lwt.return response))

(* client *)
let uri = Uri.of_string "http://localhost:8000"
let raw_content = String.make 100_000 'a'

let rec client () =
  let body = Piaf.Body.of_string raw_content in
  let* post = Piaf.Client.Oneshot.post ~body uri in
  let* () =
    match post with
    | Ok response -> (
        let* drain = Piaf.Body.drain response.body in
        match drain with
        | Ok () -> Lwt.return_unit
        | Error _error -> Lwt.return_unit)
    | Error _error -> Lwt.return_unit
  in
  client ()

let main () =
  let* _server = server () in
  client ()

let () = Lwt_main.run (main ())
