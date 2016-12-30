// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

let dayOfTheYear =
    seq {
        let months =
            [
                "Jan"; "Feb"; "Mar"; "Apr"; "May"; "Jun";
                "Jul"; "Aug"; "Sep"; "Oct"; "Nov"; "Dec"
            ]
        let daysInMonth month =
            match month with
            | "Feb"
                -> 28
            | "Apr" | "Jun" | "Sep" | "Nov"
                -> 30
            | _ -> 31
        for month in months do
            for day = 1 to daysInMonth month do
                yield (month, day)
         }

type Result = Succes of float | DivByZero

let divide x y =
    match y with
    | 0.0 -> DivByZero
    | _ -> Succes(x / y)

type DefinedBuilder() =
    member this.Bind((x : Result), (rest : float -> Result)) =
        match x with
        | Succes(x) -> rest x
        | DivByZero -> DivByZero

    member this.Return(x : 'a) = x

let defined = DefinedBuilder()

let totalResistance r1 r2 r3 =
    defined {
        let! x = divide 1.0 r1
        let! y = divide 1.0 r2
        let! z = divide 1.0 r2
        return divide 1.0 (x + y + z)
    }

let totalResistance' r1 r2 r3 =
    defined.Bind(
        (divide 1.0 r1), 
        (fun x ->
            defined.Bind(
                (divide 1.0 r2),
                (fun y ->
                    defined.Bind(
                        (divide 1.0 r3),
                        (fun z -> 
                            defined.Return(
                                divide 1.0 (x + y + z)
                            )
                        )
                    )
                )
            )
        )
    )



[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    0 // return an integer exit code
