module Frame = struct
  type 'a t = { v : 'a Option.t; time : float }

  let empty = { v = None; time = 0. }
  let create default = { v = default; time = 0. }
  let _step t0 t1 ~d = if d <= 0. then t1 else t0

  (* interpolate two key frames *)
  let interp (t0 : 'a t) (t1 : 'a t) ~d = if d <= 0. then t1 else t0

  (* project a given key frame some duration into the future or past *)
  let project (t : 'a t) ~d:_ = t
  let time t = t.time
  let value t = t.v
end

type 'a t = { frames : 'a Frame.t list }

let empty = { frames = [] }

(* total duration of timeline *)
let duration (t : 'a t) =
  match List.rev t.frames with [] -> 0. | hd :: _ -> Frame.time hd

(* get interpolated frame at provided timestamp *)
let get_frame (t : 'a t) ~(time : float) =
  let rec _search (frames : 'a Frame.t list) =
    match frames with
    | hd0 :: hd1 :: _ when Frame.time hd0 <= time && Frame.time hd1 > time ->
        let start =
          Frame.interp hd0 hd1 ~d:(Frame.time hd1 -. Frame.time hd0)
        in
        Frame.project start ~d:(time -. Frame.time hd0)
    | hd :: [] when Frame.time hd <= time ->
        Frame.project hd ~d:(time -. Frame.time hd)
    | _ :: tl -> _search tl
    | [] -> Frame.empty
  in
  _search t.frames

let get (t : 'a t) ~(time : float) = Frame.value (get_frame t ~time)

(* insert selected frames into attribute timeline *)
let insert_frames (t : 'a t) ~(frames : 'a Frame.t list) =
  {
    frames =
      List.sort
        (fun (k0 : 'a Frame.t) (k1 : 'a Frame.t) ->
          Float.compare (Frame.time k0) (Frame.time k1) )
        (t.frames @ frames);
  }

(* remove all frames after supplied timestamp *)
let cut (t : 'a t) ~(time : float) =
  {
    frames = List.filter (fun (f : 'a Frame.t) -> Frame.time f <= time) t.frames;
  }
