module Range =
struct

  (* Expressive type to indicate whether a range travels forward or backwards. *)

  type range_direction = 
  | Forward 
  | Back
  
  type t = 
  | Range of { 
      direction : range_direction; 
      start_point : int; 
      end_point : int; 
      step_by : int }

  let make s e b =
    let d = if s > e then Back else Forward 
    in
    Range { direction=d; start_point=s; end_point=e; step_by=b }

  let next =
    function
    | Range { direction=Forward; start_point=s; end_point=e; step_by=by } -> 
       Range { direction=Forward; start_point=(s + by); end_point=e; step_by=by }
    | Range { direction=Back; start_point=s; end_point=e; step_by=by } -> 
       Range { direction=Back; start_point=(s - by); end_point=e; step_by=by }

  let iteri f =
    let rec iteri' f i =
      function
      | Range { direction=Forward; start_point=s; end_point=e; step_by=_ } when s > e -> ()
      | Range { direction=Back; start_point=s; end_point=e; step_by=_ } when s < e -> ()
      | Range { direction=Forward; start_point=s; end_point=e; step_by=_ } as range when s <= e -> 
         (f i s; 
          iteri' f (i + 1) (next range))
      | Range { direction=Back; start_point=s; end_point=e; step_by=_ } as range when s >= e -> 
         (f i s;
          iteri' f (i + 1) (next range))
    in
      iteri' f 0
  
  let iter f =
    iteri (fun _ x -> f x)

  let mapi f =
    let rec mapi' f i =
      function
      | Range { direction=Forward; start_point=s; end_point=e; step_by=_ } when s > e -> []
      | Range { direction=Back; start_point=s; end_point=e; step_by=_ } when s < e -> []
      | Range { direction=Forward; start_point=s; end_point=e; step_by=_ } as range when s <= e -> 
         (f i s :: mapi' f (i + 1) (next range))
      | Range { direction=Back; start_point=s; end_point=e; step_by=_ } as range when s >= e -> 
         (f i s :: mapi' f (i + 1) (next range))
    in
      mapi' f 0
  
  let map f = 
    mapi (fun _ x -> f x)
    
end
