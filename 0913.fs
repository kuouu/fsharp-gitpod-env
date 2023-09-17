namespace class0913

module list = 
    let list = [2;1]

module exercise1 = 
    let numbers = [1..6]
    let val1 :: val2 = numbers
    let val3 = val1 + 9
    let moreNumbers = 0 :: numbers
    let evenMoreNumbers = moreNumbers @ [7..9] @ [val3]
    printfn "%A" evenMoreNumbers


namespace a1

module Products = 

    type Colour = Red | Pink | Violet | Blue | Black
    type Brand = Revlon | Kiko
    type Product = Lipstick of Colour * Brand | NailPolish of Colour | Mascara
    // Define pricing function
    let getPrice (product: Product) =

        match product with
        | Lipstick (colour, brand) ->
            match colour, brand with
            | violet, Kiko -> 10.99
            | pink, _ -> 11.49
            | red, Revlon -> 13.99
            | _ -> 9.99
        | NailPolish colour ->
            match colour with
            | Red | Blue -> 9.99
            | _ -> 8.99
        | Mascara -> 7.99
        