let uri = Uri.of_string "http://localhost:8080"
let ( let* ) = Lwt.bind

let main n =
  let* client = Piaf.Client.create uri in
  let client = Result.get_ok client in
  let requests =
    List.init n (fun _ ->
        let open Piaf in
        let body = Body.of_string "Hello" in
        let* response = Client.post client ~body "/" in
        let response = Result.get_ok response in
        let* drain = Body.drain response.body in
        let () = Result.get_ok drain in
        Lwt.return_unit)
  in
  Lwt_list.iter_p (fun x -> x) requests

let () = Lwt_main.run (main 8)
