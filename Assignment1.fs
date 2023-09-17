(*
Implement in F# a system that calculates how much commission someone has earned, 
based on what they’ve bought and sold for a cosmetics company. You'll find the 
specification below:

- There are two attributes that can be associated with a product: colour and brand.
- Catalog is reflected as a composition of types: colour (red, pink, violet, blue, 
  black), brand (Revlon, Kiko), product (lipstick, nail-polish, mascara).
- Products have different varieties: lipsticks can have different colours and brands; 
  nail polish comes in different colours. Mascara comes in only one variety: black, 
  no brand.
- Pricing function is based on a fixed brand/colour/product combination provided below 
  and returns a product cost.
- Selling price function adds 35% mark up for each product.
- Each employee has a name, a total value of bought goods as well as a total value of 
  sold goods associated.
- System allows to create a new employee with a given name and the value of sold and 
  bought items equal to zero.
- An employee can buy or sell n units of a given product.
- Commission calculation is performed per employee, based on the profits made (value 
  of items sold - value of items bought) and the flat commission rate (35%). The 
  function should return the name of the salesperson and their commission (except if 
  it the profit is negative - the commission is then 0).

For testing purposes, add lines of code that simulate the scenario where a sales person 
called "Andrew" is employed and executed the following transactions:

1) bought 5 red Revlon lipsticks
2) bought 18 mascaras
3) bought 3 black nail polish
13.99 * 5 + 7.99 * 18 + 8.99 * 3 = 240.74
4) sold 3 red Revlon lipsticks
5) sold 3 black nail polish
6) sold 16 mascaras
(13.99 * 3 + 8.99 * 3 + 7.99 * 16) * 1.35 = 265.653

(265.653 - 240.74) * 0.35 = 24.913

*)

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
        let profit = employee.Sold - employee.Bought
        if profit > 0.0 then
            employee.Name, profit * 0.35
        else
            employee.Name, 0.0

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
