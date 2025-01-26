module Interp = struct
  type t = Linear | Step | Smooth
end

module Frame = struct
  type t = {
    v : float;
    dv : float;
    ddv : float;
    dddv : float;
    time : float;
    interp : Interp.t;
  }

  let empty =
    { v = 0.; dv = 0.; ddv = 0.; dddv = 0.; time = 0.; interp = Interp.Linear }

  let _step t0 t1 ~d = if d <= 0. then t1 else t0

  let _linear t0 t1 ~d =
    if d <= 0. then t1
    else { t0 with dv = (t1.v -. t0.v) /. d; ddv = 0.; dddv = 0. }

  let _smooth (t0 : t) (t1 : t) ~d =
    if d <= 0. then t1
    else
      let a =
        -2.0
        *. ((3.0 *. t0.v) -. (3.0 *. t1.v) +. (d *. ((2.0 *. t0.dv) +. t1.dv)))
        /. (d ** 2.)
      in
      let j =
        6.0
        *. ((2.0 *. t0.v) -. (2.0 *. t1.v) +. (d *. (t0.dv +. t1.dv)))
        /. (d ** 3.)
      in
      { t0 with ddv = a; dddv = j }

  (* interpolate two key frames *)
  let interp (t0 : t) (t1 : t) ~d =
    let f =
      match t0.interp with
      | Interp.Linear -> _linear
      | Interp.Smooth -> _smooth
      | Interp.Step -> _step
    in
    f t0 t1 ~d

  (* project a given key frame some duration into the future or past *)
  let project (t : t) ~d =
    let d1 = d in
    let d2 = (d ** 2.) /. 2.0 in
    let d3 = (d ** 3.) /. 6.0 in
    {
      t with
      v = t.v +. (t.dv *. d1) +. (t.ddv *. d2) +. (t.dddv *. d3);
      dv = t.dv +. (t.ddv *. d1) +. (t.dddv *. d2);
      ddv = t.ddv +. (t.dddv *. d1);
      dddv = t.dddv;
    }

  let time t = t.time
  let value t = t.v
end

type t = { frames : Frame.t list }

let empty = { frames = [] }

(* total duration of timeline *)
let duration (t : t) =
  match List.rev t.frames with [] -> 0. | hd :: _ -> Frame.time hd

(* get interpolated frame at provided timestamp *)
let get_frame (t : t) ~(time : float) =
  let rec _search (frames : Frame.t list) =
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

let get (t : t) ~(time : float) = Frame.value (get_frame t ~time)

(* insert selected frames into attribute timeline *)
let insert_frames (t : t) ~(frames : Frame.t list) =
  {
    frames =
      List.sort
        (fun (k0 : Frame.t) (k1 : Frame.t) ->
          Float.compare (Frame.time k0) (Frame.time k1) )
        (t.frames @ frames);
  }

(* remove all frames after supplied timestamp *)
let cut (t : t) ~(time : float) =
  { frames = List.filter (fun (f : Frame.t) -> Frame.time f <= time) t.frames }
