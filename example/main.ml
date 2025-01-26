open Timeline

let c =
  let open Continuous in
  let t = empty in
  let frame time v : Frame.t =
    { Frame.empty with time; v; interp = Interp.Smooth }
  in
  let t =
    insert_frames t
      ~frames:
        [ frame 0. 5.; frame 2. 0.; frame 4. 5.; frame 6. 10.; frame 8. 5. ]
  in
  let f = get_frame t ~time:5. in
  assert (Float.equal f.v 7.5)

let d =
  let open Discrete in
  let t = empty in
  let frame time v : string Frame.t = { time; v = Some v } in
  let t =
    insert_frames t
      ~frames:
        [ frame 0. "1"; frame 2. "2"; frame 4. "3"; frame 6. "4"; frame 8. "5" ]
  in
  let f = get_frame t ~time:5. in
  match f.v with None -> assert false | Some v -> assert (String.equal v "3")
