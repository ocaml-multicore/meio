include Jv.Jarray

let null_arr i =
  let arr = create i in
  for i = 0 to i - 1 do 
    set arr i Jv.null
  done;
  arr

let push t v =
  let _ : Jv.t = Jv.call t "push" [| v |] in ()

let copy t =
  Jv.call t "slice" [||]

let insert idx t v =
  let _ : Jv.t = Jv.call t "splice" [| Jv.of_int idx; Jv.of_int 0; v |] in ()

module Syntax = struct
  let ( .:[] ) t idx = get t idx
  let ( .:[]<- ) t idx v = set t idx v
end