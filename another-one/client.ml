let ( let* ) = Lwt.bind

let uri = Uri.of_string "http://localhost:8000"
let raw_content = String.make 100_000 'a'

let rec client () =
  let body = Piaf.Body.of_string raw_content in
  let* post = Piaf.Client.Oneshot.post ~body uri in
  let* () =
    match post with
    | Ok response -> (
        let* raw_content = Piaf.Body.to_string response.body in
         match raw_content with
         | Ok raw_content ->
            Format.eprintf "echo: %d\n%!" (String.length raw_content);
            Lwt.return_unit
         | Error _error -> assert false)
    | Error _error -> assert false
  in
  client ()

let () = Lwt_main.run (client ())
