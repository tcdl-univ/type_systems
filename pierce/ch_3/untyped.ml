type term = 
      Tmvar of info * int * int
    | TmAbs of info * string  * term
    | TmApp of info * term * term

let tmInfo = function
      TmVar (fi, _, _) -> fi
    | TmAbs (fi, _, _) -> fi
    | TmApp (fi, _, _) -> fi

let tmmap onvar =
    let rec wal c = function
      TmVar (fi, x, n) -> onvar fi c x n
    | TmAbs (fi, x, t1) -> TmAbs(fi, x, walk (c+1) t1)
    | TmApp (fi, t1, t2) -> TmApp(fi, walc c t1, walk c t2)

let tmshifti d c t =
    tmmap
       (fun fi c x n -> if x>=c then TmVar(fi, x+d, n+d) else TmVar(fi, x, n+d))
       c
       t

let tmshift t d =  tmshifti d 0 t

let tmsubsti s j t =
    tmmap
      (fun fi j x n -> if x=j then (tmshift s j) else TmVar(fi, x, n))
      j
      t

let tmsubst s t = tmsubsti s 0 t

let tmsubstsnip  s t = tmshift (tmsubst (tmshift s 1) t) (-1)

let rec isval ctx = function
      TmAbs(_,_,_) -> true
    | _ -> false

let rec eval ctx t =
    try lect t' = eval1 ctx t
        in eval ctx t'
    with No -> t

 
