open OS  (* provides Time, Console and Main *)
open Lwt

module Int64extentlist = ExtentlistSet.ExtentlistSet(struct type t=int64 with rpc let add=Int64.add let sub=Int64.sub let zero=0L end)
	
let write_char page blkif c start len =
	let start = Int64.to_int start in
	lwt () = Console.log_s (Printf.sprintf "write_char: %c %d %d\n%!" c start len) in
	Io_page.string_blit (String.make 4096 c) page;
	for_lwt i=start to start+len-1 do
	blkif#write_page (Int64.of_int (i*8)) page
    done

let write_junk blkif size n current_junk =
	let page = Io_page.get () in
	let maxsize = 256L in (* in pages *)
	let rec inner m cur =
        if m=n then Lwt.return cur else
            let char = Char.chr (Random.int 255) in
            let start = Random.int64 (Int64.sub size maxsize) in
            let len = Random.int (Int64.to_int maxsize) in
            lwt () = write_char page blkif char start len in
            let myextentlist = [(start,Int64.of_int len)] in
            inner (m+1) ((myextentlist,char)::(List.map (fun (extlist,c) -> 
				(Int64extentlist.difference extlist myextentlist, c)) cur))
	in
	inner 0 current_junk
		
(*let check_junk file junk =
    let fd = Unix.openfile file [Unix.O_RDONLY] 0o000 in
	let page = Io_page.get () in
    Pervasiveext.finally (fun () -> 
        let rec inner j =
            match j with 
                | (extentlist,c)::rest ->
                    Printf.fprintf stderr "Checking char code=%d (no. of extents: %d)\n%!" (Char.code c) (List.length extentlist);
                    List.iter (fun (start,len64) -> 
                        let len = Int64.to_int len64 in
                        ignore(Unix.LargeFile.lseek fd start Unix.SEEK_SET);
                        let s = Unixext.really_read_string fd len in
                        let check = String.make len c in
                        Printf.fprintf stderr "String at pos %Ld len %Ld..." start len64;
                        assert(String.compare s check = 0);
                        Printf.fprintf stderr "OK!\n%!") extentlist;
                    inner rest
                | _ -> ()
        in 
        try
            inner junk 
        with e -> 
            Printf.fprintf stderr "Not OK!\n%!";
            raise e)
        (fun () ->
            Unix.close fd)
  *)      
