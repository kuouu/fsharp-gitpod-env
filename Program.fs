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

type Exchange = {
    Name: string
    Url: string
}

module CrossTradedCurrencyPair =
    let exchangeList = [
        {Name = "Bitfinex"; Url = "https://www.bitfinex.com/"};
        {Name = "Kraken"; Url = "https://www.kraken.com/"};
        {Name = "Bitstamp"; Url = "https://www.bitstamp.net/"}
    ]
    let retrieveCryptoCurrencyPairListFromExchanges (exchange: Exchange)   = 
        printfn "Crypto currency pair list retrieved from %s" exchange.Name
        let fakeCryptoPairs = ["ETH-USD"; "BTC-USD"; "CHZ-USD"]
        fakeCryptoPairs
    let identifyCommonCrossTradedCurrencyPair (pairs: list<list<string>>) = 
        List.reduce (
            fun (acc: string list) (currentSet: string list) -> 
                Set.toList <| Set.intersect (Set.ofList acc) (Set.ofList currentSet)
        ) pairs
    let storeCrossTradedCurrencyPair (pairs: string list) =
        printfn "Cross traded currency pair stored"
    let identifyCrossTradedCurrencyPair = 
        // RetrieveCryptoCurrencyPairListFromExchanges
        let crossTradedCurrencyPairs = 
            List.map (fun exchange -> retrieveCryptoCurrencyPairListFromExchanges exchange) exchangeList
        // IdentifyCommonCrossTradedCurrencyPair
        let crossTradedPairs = identifyCommonCrossTradedCurrencyPair crossTradedCurrencyPairs
        // StoreCrossTradedCurrencyPair
        storeCrossTradedCurrencyPair crossTradedPairs
