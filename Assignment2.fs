namespace Assignment2

module recursive  = 
    let cardNumber: int list = [4;7;1;6;6;6;3;0;3;6;5;5;4;1;2;3];
    let rec reverseDigit digits =
        match digits with
        | [] -> []
        | x::xs -> reverseDigit xs @ [x]
    let rec doubleDigit digits =
        match digits with
        | [] -> []
        | [x] -> [x]
        | x::y::xs -> x::(y*2)::doubleDigit xs
    let rec sumDigits digits =
        match digits with
        | [] -> 0
        | x::xs -> x + sumDigits xs
    match (cardNumber |> reverseDigit  |> doubleDigit |> sumDigits) % 10 with
    | 0 -> printfn "Valid"
    | _ -> printfn "Invalid"

module higherOrderFunc  = 
    let cardNumber: int list = [4;7;1;6;6;6;3;0;3;6;5;5;4;1;2;3];
    let calculateDigit (cardNumber: int list) : string = 
        cardNumber
        |> List.rev
        |> List.mapi (fun i x -> match i % 2 with | 0 -> x | _ -> x*2)
        |> List.sum
        |> fun x -> match x % 10 with | 0 -> "Valid" | _ -> "Invalid";
    printfn "%s" (calculateDigit cardNumber);


(*
    Reflection:
    In this assignment, we perform the same task using recursive function and higher order function.
    In the recursive function, I create three functions to perform the task, and connect them together
    to get the result. 

    On the other hand, I use List.rev, List.mapi, List.sum to perform the same task in the higher order
    function. It is more concise and intuitive. So, I would prefer higher order function in this situation.

    However, if the problem is more complex, like dealing with tree or graph structure, I would prefer
    recursive function. It would be more suitable for the situation that the problem contain nested structure.
*)