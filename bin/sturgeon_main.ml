open Sturgeon
open Inuit

type mode = [ `Words | `Blocks | `Allocations ]

let mode_to_string = function
  | `Words -> "live words"
  | `Blocks -> "live blocks"
  | `Allocations -> "allocated words"

let modes = [ `Words; `Blocks; `Allocations ]

module Model = struct

  type t =
    { series : Series.t
    ; snapshot_index : int
    ; snapshot : Snapshot.t
    ; projects : (Address.t * string) list
    ; summary : (Address.t * string * int) list
    ; total : int
    ; mode : mode
    }

  let create series ~mode ~index projects =
    let snapshots = Series.snapshots series in
    let snapshot = List.nth snapshots index in
    let snapshot =
      List.fold_left
        (fun acc (addr, _) -> Snapshot.project acc addr)
        snapshot
        (List.rev projects)
    in
    let locations = Snapshot.locations snapshot in
    let summary = Snapshot.to_summary_list ~mode locations snapshot in
    let total =
      List.fold_left (fun sum (_, _, value) -> sum + value) 0 summary
    in
    { series
    ; snapshot_index = index
    ; snapshot
    ; projects
    ; summary
    ; total
    ; mode
    }

  let update view ?(mode=view.mode) ?(projects=view.projects) () =
    create ~mode view.series ~index:view.snapshot_index projects
end

module View = struct
  type cursor = Stui.flag Cursor.cursor

  type t = {
    tabs   : cursor;
    status : cursor;
    stack  : cursor;
    rows   : cursor;
  }

  open Cursor

  let setup c =
    text c "Display:";
    let tabs = sub c in
    text c "\n";
    let status = sub c in
    text c "\n\nStack:";
    let stack = sub c in
    text c "\n\nCallsite:\n";
    let rows = sub c in
    { tabs; status; stack; rows }

  let draw view =
    let open Model in
    let rec redraw model =
      begin (* Tabs *)
        clear view.tabs;
        let tab mode =
          text view.tabs " ";
          if model.mode = mode then
            printf view.tabs " %s " (mode_to_string mode)
          else
            link view.tabs "[%s]" (mode_to_string mode)
              (fun _ -> redraw (update ~mode model ()));
          text view.tabs " "
        in
        List.iter tab modes;
      end;
      begin (* Status line *)
        clear view.status;
        printf view.status
          "Time %f, total %d %s"
          (Snapshot.time model.snapshot) model.total
          (mode_to_string model.mode);
      end;
      begin (* Projection Stack *)
        clear view.stack;
        let rec aux = function
          | (_, s) :: projects ->
            aux projects;
            text view.stack "\n- ";
            link view.stack "%s" s
              (fun _ -> redraw (update ~projects model ()))
          | [] -> ()
        in
        if model.projects = [] then
          text view.stack " (empty)."
        else aux model.projects
      end;
      begin (* Rows *)
        clear view.rows;
        let b_or_w =
          match model.mode with
          | `Words | `Allocations -> "w"
          | `Blocks -> "b"
        in
        let print_row (addr, key, value) =
          if value > 0 then begin
            let percentage = float(value * 100) /. float(model.total) in
            printf view.rows "%5.2f%% %10d %s  " percentage value b_or_w;
            link view.rows "%s"  key
              (fun _ ->
                 let projects = (addr, key) :: model.projects in
                 redraw (update model ~projects ()));
            text view.rows "\n"
          end
        in
        List.iter print_row model.summary
      end
    in
    redraw
end

let unmarshal_profile file : Spacetime_lib.Series.t =
  let ic = open_in_bin file in
  match Marshal.from_channel ic with
  | data -> close_in ic; data
  | exception exn -> close_in ic; raise exn

let () =
  Sturgeon_recipes_command.text_command @@ fun ~args:_ shell ->
  let prompt = "Open spacetime profile (spacetime-*): " in
  Stui.read_file_name shell ~prompt @@ function
  | Error _ -> ()
  | Ok filename ->
    let name =
      match Filename.basename filename with
      | exception _ -> "spacetime"
      | basename -> Printf.sprintf "spacetime(%s)" basename
    in
    let buffer = Stui.create_buffer shell ~name in
    let cursor = Stui.open_cursor buffer in
    let header = Cursor.sub cursor in
    Cursor.text cursor "\n\n";
    let view = View.setup cursor in
    let inverted = ref false in
    let serie = lazy (
      let processed = Filename.check_suffix filename ".p" in
      if processed
      then unmarshal_profile filename
      else Spacetime_lib.Series.create filename
    ) in
    let render () =
      match
        let lazy serie = serie in
        Model.create ~mode:`Words ~index:0
          (Series.initial serie ~inverted:!inverted) []
      with
      | model -> View.draw view model
      | exception exn ->
        let cursor = view.View.status in
        Cursor.clear cursor;
        Cursor.printf cursor "Failed to process serie: %s"
          (Printexc.to_string exn)
    in
    Cursor.printf header "Viewing profile: %s\n" filename;
    let invert_text () = if !inverted then "[Inverted]" else "[Not inverted]" in
    Cursor.link header "%s" (invert_text ()) (fun c ->
        inverted := not !inverted;
        Cursor.clear c;
        Cursor.text c (invert_text ());
        render ()
      );
    render ()
