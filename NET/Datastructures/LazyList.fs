// STATE: beta - might be unstable

type 'a LazyList =
    | Cons of 'a * Lazy<'a LazyList>
    | Empty
    override this.ToString() =
        match this with
        | Empty -> "[]"
        | Cons (h, tl) ->
            sprintf "%A :: %A" h tl

module LazyList =

    /// prepends h in front of tl cons
    let (<+>) h tl = 
        Cons (h, tl)

    let private value (l : 'a Lazy) =
        l.Value

    /// fails if the list is empty
    let head (ls : 'a LazyList) =
        match ls with
        | Empty       -> failwith "could not get head of empty list"
        | Cons (h, _) -> h

    /// fails if the list is empty
    let tail (ls : 'a LazyList) =
        match ls with
        | Empty        -> failwith "could not get tail of empty list"
        | Cons (_, tl) -> tl.Value

    /// lazy foldR - IMPORTANT: will build up a lazy computations that might bust the stack if forced
    let rec foldBack (f : 'a -> 's Lazy -> 's Lazy) (ls : 'a LazyList) (init : 's Lazy) =
        match ls with
        | Empty -> init
        | Cons (l, ls) ->
            f l (lazy (foldBack f (ls.Value) init) |> value)

    /// build up a new lazy-list
    let rec unfold (next : 's -> 'a * 's Lazy) (stop : 's -> bool) (init : 's) : 'a LazyList =
        if stop init then Empty else
        let (x, s') = next init
        Cons (x, lazy (unfold next stop s'.Value))

    /// lazily appends two lists
    let rec append (xs : 'a LazyList) (ys : 'a LazyList Lazy) =
        match xs with
        | Empty      -> ys.Value
        | Cons(x,xs) -> Cons (x, lazy (append xs.Value ys))

    /// monadic-return (wraps a value)
    let singleton v =
        Cons (v, lazy Empty)

    /// monadic-bind for a lazy list (uses foldBack - so StackOverflows are possible)
    let rec concatMap (f : 'a -> 'b LazyList) (xs : 'a LazyList) : 'b LazyList =
        match xs with
        | Empty -> Empty
        | Cons (x,xs) ->
            append (f x) (lazy (concatMap f xs.Value))

    /// maps list-items i to f i, keeping the element count
    let rec map f (ls : 'a LazyList) =
        match ls with
        | Empty -> Empty
        | Cons (h, tl) ->
            f h <+> lazy (map f tl.Value)

    /// only those list-items i where f i is true
    let rec filter f (ls : 'a LazyList) =
        match ls with
        | Empty -> Empty
        | Cons (h, tl) ->
            let tl' = lazy (filter f tl.Value)
            if f h
            then h <+> tl'
            else tl'.Value

    /// flattens the lists
    let rec flatten (xss : 'a LazyList LazyList) =
        match xss with
        | Empty        -> Empty
        | Cons(xs,xss) -> append xs (lazy (flatten xss.Value))

    /// takes the first n items of xs
    let rec take (n : int) (xs : 'a LazyList) =
        if n <= 0 then Empty else
        match xs with
        | Empty        -> Empty
        | Cons (x, xs) -> Cons (x, lazy take (n-1) xs.Value)

    /// repeats x forever
    let repeat x = 
        let u = lazy ()
        unfold (fun _ -> (x, u)) (fun _ -> false) ()

    /// cylces the xs forever
    let cycle xs =
        repeat xs
        |> flatten


    /// zips two lazy lists using op
    let rec zipWith op (xs : 'a LazyList) (ys : 'b LazyList) =
        match (xs, ys) with
        | Empty, _
        | _, Empty ->
            Empty
        | (Cons(x,xs), Cons(y,ys)) ->
            op x y <+> lazy (zipWith op xs.Value ys.Value)

    /// creates a lazy list from a common list
    let rec fromList (ls : 'a list) : 'a LazyList =
        match ls with
        | []      -> Empty
        | (l::ls) -> l <+> lazy (fromList ls)

    /// transforms a lazy list into a common list
    let toList (ls : 'a LazyList) =
        foldBack (fun a tl -> lazy (a :: tl.Value)) ls (lazy [])
        |> value

    /// creates a lazy list from a sequence
    let fromSeq (ls : 'a seq) : 'a LazyList =
        let enum = ls.GetEnumerator()
        let rec create () =
            if enum.MoveNext()
            then enum.Current <+> lazy create ()
            else Empty
        create ()

    /// transforms a lazy list into a sequence
    let rec toSeq (ls : 'a LazyList) : 'a seq =
        match ls with
        | Empty -> Seq.empty
        | Cons (h,tl) ->
            seq {
                yield h
                yield! toSeq tl.Value
            }

    type LazyListBuilder internal () =
        member x.Bind(m, f)    = concatMap f m
        member x.Return(v)     = singleton v
        member x.ReturnFrom(v) = v
        member x.Delay(f)      = f ()

    let Do = LazyListBuilder ()

module Examples =
    open LazyList

    let rec sieve = function
        | Empty -> Empty
        | Cons (p,cs) ->
            let ps = lazy (
                cs.Value
                |> filter (fun c -> c % p > 0I)
                |> sieve)
            p <+> ps

    let integers : bigint LazyList =
        unfold (fun n -> (n,lazy (n+1I))) (fun _ -> false) 1I

    let primes = sieve (tail integers)

    let show n ls =
        ls |> toSeq
           |> Seq.take n
           |> Seq.iter (printf "%A, ")

    let rec fibs =
        1I <+> lazy (1I <+> lazy (zipWith (+) fibs (tail fibs)))

    let mult (n : bigint) = repeat n |> take (int n)

    let pyramid = concatMap mult integers