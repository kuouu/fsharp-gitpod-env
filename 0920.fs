namespace class0920

module ex1 = 
    type ProductType = {
        product: string;
        price: float;
    }
    type CustomerType = {
        name: string;
        purchases: ProductType list;
    }
    let customer1 = {
        name = "John";
        purchases = [
            {product = "Lipstick"; price = 10.99};
            {product = "Mascara"; price = 7.99};
            {product = "NailPolish"; price = 8.99}
        ];
    }

    let getHighestPurchase customer = 
        List.reduce(fun acc x -> if acc.price > x.price then acc else x) customer.purchases
