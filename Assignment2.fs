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
    let doubleDigit = [for i in 1..cardNumber.Length -> match i % 2 with | 1 -> cardNumber.[cardNumber.Length - i] | _ -> cardNumber.[cardNumber.Length - i]*2]
    let sum = doubleDigit |> List.sum;
    match sum % 10 with
    | 0 -> printfn "Valid"
    | _ -> printfn "Invalid"


(*
    Reflection:
    In this assignment, I would prefer recursive function over higher order function. Due to the assignment requirement, 
    I have to use collections modules that only taught in the class to implement. Even it is shorter than the recursive 
    function, I think recursive function is more readable and easier to understand in this situation.

    If there is no restriction to use collections modules, I would prefer higher order function. It would be more
    concise and easier to understand. Only one mapi and reduce function can solve the problem. As for the recursive
    function, it would be more suitable for the situation that the problem contain nested structure, like dealing with 
    tree or graph structure.
*)