let ( let* ) = Lwt.bind
let listen_address = Unix.(ADDR_INET (inet_addr_loopback, 8000))

let server () =
  Lwt_io.establish_server_with_client_socket listen_address
    (Piaf.Server.create ?config:None ?error_handler:None
       (fun { ctx = _; request } ->
         let body = request.Piaf.Request.body in
         let* raw_content = Piaf.Body.to_string body in
         match raw_content with
         | Ok raw_content ->
            Format.eprintf "received: %d\n%!" (String.length raw_content);
            let response = Piaf.Response.of_string ~body:raw_content `OK in
            Lwt.return response
         | Error _error -> assert false))

let main () = 
  let _server = server () in
  let promise, _resolver = Lwt.wait () in
  promise

let () = Lwt_main.run (main ())
