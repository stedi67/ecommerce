namespace ECommerce

module TaxAddress =
    type ISOCountryCode = ISOCountryCode of string

    type Zip = Zip of string

    type State = State of string

    type TaxAddress = {
        country: ISOCountryCode
        state: State option
        zip: Zip option
    }

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
            | Ok country -> Ok {country=country; state=state; zip=zip}
            | Error error -> Error error


module TaxLib =

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

    type OneCountryOneState = {
        country: ISOCountryCode
        state: State
    }

    type OneCountryOneStateWithZips = {
        country: ISOCountryCode
        state: State
        zipCodes: Zip list
    }

    type TaxZone =
        | OneCountryOneStateWithZips of OneCountryOneStateWithZips
        | OneCountryOneState of OneCountryOneState
        | OneCountry of ISOCountryCode
        | SeveralCountries of ISOCountryCode list
        | RestOfWorld


    type TaxInfoId = TaxInfoId of int

    type TaxInfo = {
        id: TaxInfoId
        zone: TaxZone
        customerType: CustomerType
        productType: ProductType
        rate: TaxRate
    }

    type TaxInfoList = TaxInfo list

    type TaxQuery = {
        address: TaxAddress
        productInfo: ProductType list
        customerType: CustomerType
    }

    let addressMatchesZone address taxInfo =
        match taxInfo.zone with
        | RestOfWorld -> true
        | SeveralCountries ccList ->
            match address with
            | {country = cc; state = _; zip = _} ->
                    List.contains cc ccList
        | OneCountry occ ->
            match address with
            | {country = cc; state = _; zip = _} ->
                    cc = occ
        | OneCountryOneState {country = occ; state = ostate} ->
            match address with
            | {country = cc; state = Some state; zip = _} ->
                cc = occ && ostate = state
            | _ -> false
        | OneCountryOneStateWithZips {country = occ; state = ostate; zipCodes = zipCodes} ->
            match address with
            | {country = cc; state = Some state; zip = Some zip} ->
                cc = occ && ostate = state && List.contains zip zipCodes
            | _ -> false


    let taxInfoOrder taxInfo =
        // the tax info should be ordered by importance since
        // several infos can map a query.
        let zoneNumber =
            match taxInfo.zone with
            | OneCountryOneStateWithZips _ -> 10
            | OneCountryOneState _ -> 20
            | OneCountry _ -> 30
            | SeveralCountries _ -> 40
            | RestOfWorld -> 50

        let productNumber =
            match taxInfo.productType with
            | Book -> 10
            | Service -> 10
            | Shipment -> 10
            | _ -> 20

        zoneNumber * productNumber


    type FindBestTaxMatch = TaxInfoList -> TaxQuery -> TaxInfo option

    let findBestTaxMatch : FindBestTaxMatch =
        fun taxInfoList taxQuery ->
            let resultList =
                taxInfoList
                    |> List.filter (fun info -> info.customerType = taxQuery.customerType)
                    |> List.filter (fun info -> addressMatchesZone taxQuery.address info)
                    |> List.filter (fun info -> List.contains info.productType taxQuery.productInfo)
                    |> List.sortBy taxInfoOrder
            match resultList with
            | fst::_ -> Some fst
            | _ -> None


// public api for the TaxLib
module TaxLibApi =

    // DTO: Data Transfer Object

    type TaxInfoDTO = {
        id: int
        rate: decimal
    }

    type TaxQueryDTO = {
        countryCode: string
        state: string option
        zipCode: string option
        productInfo: string list
        customerType: string
    }
