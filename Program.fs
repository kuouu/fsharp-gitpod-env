type Order = {
    Id: int
    CurrencyPair: string
    Amount: float
}

type OrderStatus = {
    Success: bool
    price: float
}

type OrderResult = {
    Success: bool
    profit: float
}

module Order =
    let placeBuyOrder (order: Order) = 
        printfn "Buy Order placed"
        { Success = true; price = 99.0 }

    let placeSellOrder (order: Order) = 
        printfn "Sell Order placed"
        { Success = true; price = 100.0 }

    let notifyUsers = 
        printfn "Users notified"

    let storeTransactionHistory (orderResult: OrderResult) = 
        printfn "Transaction history stored"

    let rec placeOrder (order: Order)  = 
        // PlaceSellAndBuyOrders
        let buyResult = placeBuyOrder order
        let sellResult = placeSellOrder order
        let orderResult = { 
            Success = buyResult.Success && sellResult.Success; 
            profit = sellResult.price - buyResult.price 
        }
        // EvaluateOrdersResult
        match orderResult with
        | {Success = false } -> // OrdersOnlyWithOneSideFilled
            notifyUsers
        | { profit = p } when p >= order.Amount -> // OrdersFulfilled
                storeTransactionHistory orderResult
        | _ -> // OrdersPartiallyFulfilled
            placeOrder {order with Amount = order.Amount - orderResult.profit} 
