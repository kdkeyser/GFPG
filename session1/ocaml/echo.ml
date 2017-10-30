(* ocamlfind c -w A -linkpkg -package lwt,lwt.unix echo.ml -o echo *)
open Lwt

let server_port = 5437

let rec handleSingleConnection inchan outchan =
  Lwt_io.read_line inchan >>= (fun msg -> 
  if (msg = "exit") then 
    Lwt_io.close outchan >>= fun _ -> Lwt_io.close inchan
  else
    Lwt_io.write_line outchan msg >>= fun _ -> handleSingleConnection inchan outchan)

let serverLoop socket =
  let rec _serverLoop () =
    Lwt_unix.accept socket >>=
      (fun (socket, _) ->
        let inchan = Lwt_io.of_fd ~mode:Lwt_io.input socket in
        let outchan = Lwt_io.of_fd ~mode:Lwt_io.output socket in
        let _t = handleSingleConnection inchan outchan in
        _serverLoop ()
      )
  in
  _serverLoop ()

let initSocket sockaddr =
  let socket = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Lwt_unix.bind socket sockaddr >>= fun () ->
  let () = Lwt_unix.listen socket 5 in
  Lwt.return socket

let lwtMain () =
  let sockaddr = Unix.ADDR_INET (Unix.inet_addr_any, server_port) in
  initSocket sockaddr >>= fun socket ->
  serverLoop socket

let main =
  Lwt_main.run (lwtMain ())
