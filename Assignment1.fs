namespace assignment1

module Products = 

    type Colour = Red | Pink | Violet | Blue | Black
    type Brand = Revlon | Kiko
    type Product = Lipstick of Colour * Brand | NailPolish of Colour | Mascara
    // Define pricing function
    let getPrice (product: Product) =

        match product with
        | Lipstick (colour, brand) ->
            match colour, brand with
            | Violet, Kiko -> 10.99
            | Red, Revlon -> 13.99
            | Pink, _ -> 11.49
            | _ -> 9.99
        | NailPolish colour ->
            match colour with
            | Red | Blue -> 9.99
            | _ -> 8.99
        | Mascara -> 7.99
        
    let getSellingPrice (product: Product) =
        getPrice product * 1.35

module Employees =

    type Employee = { Name: string; Bought: float; Sold: float }
    let createEmployee name = { Name = name; Bought = 0.0; Sold = 0.0 }
    let buy (employee: Employee) (product: Products.Product) (quantity: int) =
        { employee with Bought = employee.Bought + Products.getPrice product * float quantity }
    let sell (employee: Employee) (product: Products.Product) (quantity: int) =
        { employee with Sold = employee.Sold + Products.getSellingPrice product * float quantity }
    let getCommission employee =
        match employee.Sold - employee.Bought with
        | profit when profit > 0.0 -> (employee.Name, profit * 0.35)
        | _ -> (employee.Name, 0.0)

module Test =
    
    // initialize an employee
    let andrew =
        Employees.createEmployee "Andrew" // Create a new employee
        |> fun employee -> Employees.buy employee (Products.Lipstick (Products.Red, Products.Revlon)) 5 // Buy 5 red Revlon lipsticks
        |> fun employee -> Employees.buy employee Products.Mascara 18 // Buy 18 mascaras
        |> fun employee -> Employees.buy employee (Products.NailPolish Products.Black) 3 // Buy 3 black nail polish
        |> fun employee -> Employees.sell employee (Products.Lipstick (Products.Red, Products.Revlon)) 3 // Sell 3 red Revlon lipsticks
        |> fun employee -> Employees.sell employee (Products.NailPolish Products.Black) 3 // Sell 3 black nail polish
        |> fun employee -> Employees.sell employee Products.Mascara 16 // Sell 16 mascaras

    // calculate the commission
    let (name, commission) = Employees.getCommission andrew
    printfn "Employee: %s, Commission: %f" name commission
