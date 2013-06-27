type mlist = Mnil | Mcons of (int ref * mlist ref)

let fold_left f acc l = 
  let rec help f acc lst = 
    match lst with 
    |Mnil -> failwith "whoops"
    |Mcons(a,b) -> 
      let temp = f acc !a in 
      if !b == l then temp 
      else help f temp !b in 
  help f acc l

let map f lst = 
  let rec help l = 
    match l with 
    |Mnil -> failwith "whoops"
    |Mcons(a,b) ->
      a:=(f !a); 
      if (!b == lst) then () else help !b in 
  help lst; lst 

let length lst = 
  let rec help l = 
    match l with 
    |Mnil -> failwith "whoops"
    |Mcons(a,b) -> 
      if !b == lst then 1
      else 1 + (help !b) in 
  help lst 

let reverse l = 
  let rec help prev curr = 
    match curr with 
    |Mnil -> failwith "whoops"
    |Mcons(a,b) -> let next = !b in b := prev; 
                   if curr == l then () else help curr next  
  in match l with 
  |Mnil -> ()
  |Mcons(a,b) -> help l !b 