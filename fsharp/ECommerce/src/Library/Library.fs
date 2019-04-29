namespace ECommerce

module TaxAddress =
    type ISOCountryCode = ISOCountryCode of string

    type Zip = Zip of string

    type State = State of string

    type TaxAddress =
        { Country: ISOCountryCode
          State: State option
          Zip: Zip option }

    let createCountry countryCode =
        if String.length countryCode = 2 then
            Ok (countryCode.ToLower() |> ISOCountryCode)
        else
            Error "Country Code needs to be a 2 char ISO country code"


    let createState state =
        if String.length state <> 0 then
            Some (State state)
        else
            None


    let createZip zip =
        if String.length zip <> 0 then
            Some (Zip zip)
        else
            None


    let createTaxAddress countryCode state zip =
        let ccResult = createCountry countryCode
        let state = createState state
        let zip = createZip zip
        match ccResult with
        | Ok country -> Ok {Country=country; State=state; Zip=zip}
        | Error error -> Error error


module Tax =

    open TaxAddress

    type Account = Account of string

    type ProductType =
        | Physical
        | NonPhysical
        | Software
        | Service
        | Shipment
        | Book

    type ProductInfo = ProductType list

    type CustomerType =
        | B2C
        | B2B
        | IC

    type TaxRate = TaxRate of decimal

    type TaxZoneLabel = TaxZoneLabel of string

    type OneCountryOneState =
        { Country: ISOCountryCode
          State: State }

    type OneCountryOneStateWithZips =
        { Country: ISOCountryCode
          State: State
          ZipCodes: Zip list }

    type TaxZone =
        | OneCountryOneStateWithZips of OneCountryOneStateWithZips
        | OneCountryOneState of OneCountryOneState
        | OneCountry of ISOCountryCode
        | SeveralCountries of ISOCountryCode list
        | RestOfWorld


    type TaxInfoId = TaxInfoId of int

    type TaxInfo =
        { Id: TaxInfoId
          Zone: TaxZone
          CustomerType: CustomerType
          ProductType: ProductType
          Rate: TaxRate }

    type TaxInfoList = TaxInfo list

    type TaxQuery =
        { Address: TaxAddress
          ProductInfo: ProductType list
          CustomerType: CustomerType }

    let addressMatchesZone address taxInfo =
        match taxInfo.Zone with
        | RestOfWorld -> true
        | SeveralCountries ccList ->
            match address with
            | {Country = cc; State = _; Zip = _} ->
                    List.contains cc ccList
        | OneCountry occ ->
            match address with
            | {Country = cc; State = _; Zip = _} ->
                    cc = occ
        | OneCountryOneState {Country = occ; State = ostate} ->
            match address with
            | {Country = cc; State = Some state; Zip = _} ->
                cc = occ && ostate = state
            | _ -> false
        | OneCountryOneStateWithZips {Country = occ; State = ostate; ZipCodes = zipCodes} ->
            match address with
            | {Country = cc; State = Some state; Zip = Some zip} ->
                cc = occ && ostate = state && List.contains zip zipCodes
            | _ -> false


    let taxInfoOrder taxInfo =
        // the tax info should be ordered by importance since
        // several infos can map a query.
        let zoneNumber =
            match taxInfo.Zone with
            | OneCountryOneStateWithZips _ -> 10
            | OneCountryOneState _ -> 20
            | OneCountry _ -> 30
            | SeveralCountries _ -> 40
            | RestOfWorld -> 50

        let productNumber =
            match taxInfo.ProductType with
            | Book -> 10
            | Service -> 10
            | Shipment -> 10
            | _ -> 20

        zoneNumber * productNumber


    type FindBestTaxMatch = TaxInfoList -> TaxQuery -> TaxInfo option

    let findBestTaxMatch : FindBestTaxMatch =
        fun taxInfoList taxQuery ->

            let cmpCustomerType = fun (info:TaxInfo) -> info.CustomerType = taxQuery.CustomerType

            let cmpAddress info =
                addressMatchesZone taxQuery.Address info

            let cmpProduct info =
                List.contains info.ProductType taxQuery.ProductInfo

            let resultList =
                taxInfoList
                |> List.filter cmpCustomerType
                |> List.filter cmpAddress
                |> List.filter cmpProduct
                |> List.sortBy taxInfoOrder
            match resultList with
            | fst::_ -> Some fst
            | _ -> None


module Money =

    type ISOCurrencyCode =
        | EUR
        | USD
        | GBP
        | CAD
        | CHF
        | JPY
        | AUD
        | NOK

    type Currency =
        { Code: ISOCurrencyCode
          Exponent: int }  

    type Money =
        { Amount: decimal
          Currency: Currency }


module Discount =

    open Money

    type DiscountType =
        | Manual
        | Campaign

    type DiscountValue =
        | DiscountFactor of decimal
        | AbsoluteDiscount of Money

    type Discount = 
        { Type: DiscountType
          Value: DiscountValue }


module Price =

    open Discount
    open Money
    open Tax

    type NetPrice = Net of Money

    type GrossPrice = Gross of Money

    type Price =
        | NetPrice
        | GrossPrice

    type PriceEvent =
        | CatalogPrice of Price
        | AppliedUnitDiscount of Discount
        | AppliedGlobalDiscount of Discount

    type LineItemPrice =
        { UnitPrice: Price
          Count: int
          Tax: TaxInfo
          EventLog: PriceEvent list }

            
// Worldpay payments
module Payment =

    open Money

    type Amount = Amount of Money

    type PaymentId = PaymentId of string

    type PaymentEvent =
        | PaymentRequested of Amount
        | PaymentAuthorized
        | PaymentCaptured
        | PaymentRefunded of Amount

    type PaymentMethod =
        | CreditCart
        | PayPal

    type Payment =
        { PaymentId: PaymentId
          PaymentMethod: PaymentMethod
          PaymentEvents: PaymentEvent list }


module Shipment =

    type Tracking = Tracking of string

    type ShipmentId = ShipmentId of int

    type ShipmentProvider =
        | Rhiem
        | Shipwire

    type ShipmentEvent =
        | ShipmentRequested
        | ShipmentAcknowledged
        | ShipmentShipped of Tracking list


    type Shipment =
        { ShipmentId: ShipmentId
          ShipmentProvider: ShipmentProvider
          ShipmentEvents: ShipmentEvent list}


module Order =

    open Price
    open Money
    open Tax

    type ArticleCode = ArticleCode of string
    type OrderId = OrderId of int

    type OrderItem =
        { ArticleCode: ArticleCode
          Price: LineItemPrice }

    type TaxGroup =
        { TaxInfo: TaxInfo
          Amount: Money
          BasedOnNetPrice: Money
        }

    type OrderSummary =
        { NetPrice: NetPrice
          GrossPrice: GrossPrice
          TaxGroups: TaxGroup list }

    type RawOrder =
        { OrderId: OrderId
          OrderItems: OrderItem list }

    type Order =
        | OpenOrder of RawOrder
        | OrderedOrder of RawOrder
        | ClosedOrder of RawOrder


// public api for the Tax
module TaxApi =

    // DTO: Data Transfer Object

    type TaxInfoDTO =
        { Id: int
          Rate: decimal }

    type TaxQueryDTO =
        { ISOCountryCode: string
          State: string option
          Zip: string option
          ProductInfo: string list
          CustomerType: string }
