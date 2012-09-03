open Lwt (* provides >>= and join *)
open OS  (* provides Time, Console and Main *)

let read_line () =
  Time.sleep (Random.float 2.5) >>
  return (String.make (Random.int 20) 'a')

let rec echo_server =
  function
  |0 -> return ()
  |num_lines ->
    lwt s = read_line () in
    Console.log s;
    echo_server (num_lines - 1)

let suspend () =
  lwt cancelled = Sched.suspend () in
  Console.log (Printf.sprintf "cancelled=%d" cancelled);
  Lwt.return cancelled

let xs_watch () = 
  lwt () = Console.log_s (Printf.sprintf "xs_watch ()") in
  let rec inner () = 
    lwt dir = Xs.t.Xs.directory "control" in
    lwt result =
      if List.mem "shutdown" dir then  begin
      lwt msg = try_lwt Xs.t.Xs.read "control/shutdown" with _ -> return "" in
      lwt () = Console.log_s (Printf.sprintf "Got control message: %s" msg) in
      match msg with
      | "suspend" -> 
        lwt () = Xs.t.Xs.rm "control/shutdown" in
        lwt _ = suspend () in
        lwt () = Console.log_s "About to read domid" in
        Xs.check Xs.t;
        lwt domid = Xs.t.Xs.read "domid" in
        lwt () = Console.log_s (Printf.sprintf "We're back: domid=%s" domid) in
        return true
      | _ -> return false
      end else return false
    in
    lwt () = Time.sleep 1.0 in
    inner ()
  in inner ()

let main () =
  Random.self_init ();
  let _ = xs_watch () in
  echo_server 1000
