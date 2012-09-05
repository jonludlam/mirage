open Lwt (* provides >>= and join *)
open OS  (* provides Time, Console and Main *)
open Printf

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

let block () =
 let finished_t, u = Lwt.task () in
  let listen_t = OS.Devices.listen (fun id ->
    OS.Devices.find_blkif id >>=
    function
    | None -> return ()
    | Some blkif -> Lwt.wakeup u blkif; return ()
  ) in
  (* Get one device *)
  lwt blkif = finished_t in
  (* Cancel the listening thread *)
  Lwt.cancel listen_t;
  printf "ID: %s\n%!" blkif#id;
  printf "Connected block device\n%!";
  printf "Total device size = %Ld\nSector size = %d\n%!" blkif#size blkif#sector_size;
  printf "Device is read%s\n%!" (if blkif#readwrite then "/write" else "-only");

  let rec inner () =
    lwt res = Lwt_stream.to_list (blkif#read_512 0L 7L) in
    let str = Io_page.to_string (List.hd res) in
    lwt () = Console.log_s (Printf.sprintf "Read a sector (%s)" (String.sub str 0 8)) in
    Time.sleep 1.0 >> inner ()
  in inner ()


let main () =
  Random.self_init ();
  let _ = xs_watch () in
  block ();
  echo_server 1000
