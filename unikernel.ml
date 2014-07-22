open Lwt

let red fmt    = Printf.sprintf ("\027[31m"^^fmt^^"\027[m")
let green fmt  = Printf.sprintf ("\027[32m"^^fmt^^"\027[m")
let yellow fmt = Printf.sprintf ("\027[33m"^^fmt^^"\027[m")
let blue fmt   = Printf.sprintf ("\027[36m"^^fmt^^"\027[m")

module Main (C: V1_LWT.CONSOLE) (S: V1_LWT.STACKV4) = struct

  let buffer = Buffer.create 1000
  let () =
    for i = 1 to 392 do
      Buffer.add_string buffer (Printf.sprintf "%d " i)
    done

  let data =
    let page = Io_page.get 1 |> Io_page.to_cstruct in
    Cstruct.blit_from_string (Buffer.contents buffer) 0 page 0 (Buffer.length buffer);
    Cstruct.sub page 0 (Buffer.length buffer)

(*     let data = Cstruct.of_string (Buffer.contents buffer) *)

  let rec write flow = function
    | 0 -> return ()
    | n -> S.TCPV4.write flow data >>= fun () -> write flow (n - 1)

  let start c s =
    S.listen_tcpv4 s ~port:8000 (fun flow ->
        let dst, dst_port = S.TCPV4.get_dest flow in
        C.log_s c (green "new tcp connection from %s %d"
                     (Ipaddr.V4.to_string dst) dst_port)
        >>= fun () ->
        write flow 1000
        >>= fun () ->
        S.TCPV4.close flow
      );

    S.listen s

end
