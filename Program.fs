open System

let rec unfold1 (ss:'T seq seq) : 'T seq =
    if Seq.isEmpty ss then // no in, no out
        Seq.empty
    else
        ss
        |> Seq.filter (not << Seq.isEmpty)      // filter the exhausted ones
        |> Seq.map Seq.head                     // yield the heads
        |> Seq.append <| (                      // append the tails
            ss
            |> Seq.filter (not << Seq.isEmpty)  // filter the exhausted ones
            |> Seq.map Seq.tail                 // yield the tails
            |> unfold1                          // recursive call
            )

let rec unfold2 (buf:'T seq) (ss:'T seq seq) : 'T seq =
    if Seq.isEmpty ss then                      // no in, no out; recursion break
        buf
    else
        let heads =
            ss
            |> Seq.filter (not << Seq.isEmpty)  // filter the exhausted ones
            |> Seq.map Seq.head                 // yield the heads
            |> Seq.append buf                   // append to buf
        let tails =
            ss
            |> Seq.filter (not << Seq.isEmpty)  // filter the exhausted ones
            |> Seq.map Seq.tail                 // yield the tails

        unfold2 heads tails
            
[<EntryPoint>]
let main argv =
    let x1 = seq {10..19}
    let x2 = seq {20..29}
    let x3 = seq {30..33} // a shorter one to make sure it doesn't break

    [x1; x2; x3]
    |> List.toSeq
    |> unfold2 Seq.empty
    |> Seq.iter (printf "%d ")

    0 // return an integer exit code
