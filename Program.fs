// Learn more about F# at http://fsharp.org

open System
type BFInstr = 
    | Incr
    | Decr
    | MoveL
    | MoveR
    | Input
    | Output
    | Loop of list<BFInstr>

let incrMem mem ptr =
    Array.set mem ptr ((Array.get mem ptr) + 1)
    mem
let decrMem mem ptr =
    Array.set mem ptr (if (Array.get mem ptr) = 0 then 0 else (Array.get mem ptr) - 1)
    mem

let rec getAsciiChar () = 
        printfn "Please press a key"
        let line = Console.ReadLine()
        if String.length line  = 0
        then line.[0] |> int
        else getAsciiChar ()

let rec eval list mem ptr = match list with 
                                | (first::instrs) -> 
                                    match first with
                                        | Incr -> eval instrs (incrMem mem ptr) ptr
                                        | Decr -> eval instrs (decrMem mem ptr) ptr
                                        | MoveL -> eval instrs mem (ptr - 1)
                                        | MoveR -> eval instrs mem (ptr + 1)
                                        | Input ->  Array.set mem ptr (getAsciiChar ())
                                                    eval instrs mem ptr
                                        | Output -> printfn "%c" ((Array.get mem ptr) |> char)
                                                    eval instrs mem ptr
                                        | Loop linstrs -> 
                                                        if mem.[ptr] = 0
                                                        then eval instrs mem ptr
                                                        else 
                                                            let (newMem, newPtr) = eval linstrs mem ptr 
                                                            eval (first::instrs) newMem newPtr
                                | [] -> (mem, ptr)
let rec parse s instrs = match s with
                            | (first::rem) -> 
                                match first with 
                                | '+' -> parse rem (Array.append instrs [|Incr|])
                                | '-' -> parse rem (Array.append instrs [|Decr|])
                                | '>' -> parse rem (Array.append instrs [|MoveR|])
                                | '<' -> parse rem (Array.append instrs [|MoveL|])
                                | ',' -> parse rem (Array.append instrs [|Input|])
                                | '.' -> parse rem (Array.append instrs [|Output|])
                                | '[' -> let (linstrs, lrem) = parse rem Array.empty
                                         parse lrem (Array.append instrs [|Loop(linstrs)|])
                                | ']' -> (instrs |> Array.toList, rem)
                                | _ -> parse rem instrs
                            | [] -> (instrs |> Array.toList, [])

[<EntryPoint>]
let main _ =
    let memory = Array.create 30 0
    let (instrs, _) = (parse (Seq.toList "++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++.>+.+++++++..+++.>++.<<+++++++++++++++.>.+++.------.--------.>+.>.") Array.empty)
    printfn "%A" instrs
    printfn "%A" (eval instrs memory 0)
    0
