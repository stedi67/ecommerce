namespace ECommerce

module TaxAddress =
    type ISOCountryCode = ISOCountryCode of string

    type ZipCode = ZipCode of string

    type StateCountry = StateCountry of string

    type CountryAndState = {
        CountryCode: ISOCountryCode
        State: StateCountry
    }

    type CountryAndStateAndZip = {
        CountryCode: ISOCountryCode
        State: StateCountry
        ZipCode: ZipCode
    }

    type TaxAddress =
        | Country of ISOCountryCode
        | CountryAndState of CountryAndState
        | CountryAndStateAndZip of CountryAndStateAndZip

    let createCountry countryCode =
        if String.length countryCode = 2 then
            Ok (countryCode.ToLower() |> ISOCountryCode |> Country)
        else
            Error "Country Code needs to be a 2 char ISO country code"

    let createState state =
        if String.length state <> 0 then
            Some (StateCountry state)
        else
            None


    let createZip zip =
        if String.length zip <> 0 then
            Some (ZipCode zip)
        else
            None


    let createTaxAddress countryCode state zip =
        let ccResult = createCountry countryCode
        let stateOption = createState state
        let zipOption = createZip zip
        match (ccResult, stateOption, zipOption) with
            | (Ok (Country cc), None, None) -> ccResult
            | (Ok (Country cc), Some st, None) -> Ok (CountryAndState {CountryCode=cc; State=st})
            | (Ok (Country cc), Some st, Some z) ->  Ok (CountryAndStateAndZip {CountryCode=cc; State=st; ZipCode=z})
            | _ -> ccResult


module TaxLib =

    open TaxAddress

    type Account = Account of string

    type ProductType =
      | Physical
      | NonPhysical
      | Software
      | Service
      | Book

    type ProductInfo = ProductType list

    type CustomerType =
      | B2C
      | B2B
      | IC

    type TaxRate = TaxRate of decimal

    type TaxZoneLabel = TaxZoneLabel of string

    type OneCountryOneState = {
        CountryCode: ISOCountryCode
        State: StateCountry
    }

    type OneCountryOneStateWithZips = {
        CountryCode: ISOCountryCode
        State: StateCountry
        ZipCodes: ZipCode list
    }

    type TaxZone =
        | OneCountryOneStateWithZips of OneCountryOneStateWithZips
        | OneCountryOneState of OneCountryOneState
        | OneCountry of ISOCountryCode
        | SeveralCountries of ISOCountryCode list
        | RestOfWorld


    type TaxInfoId = TaxInfoId of int

    type TaxInfo = {
        Id: TaxInfoId
        Zone: TaxZone
        CustomerType: CustomerType
        ProductType: ProductType
        Rate: TaxRate
    }

    type TaxInfoList = TaxInfo list

    type TaxQuery = {
        Address: TaxAddress
        ProductInfo: ProductType list
        CustomerType: CustomerType
    }

    type FindBestTaxMatch = TaxInfoList -> TaxQuery -> TaxInfo option

    let matchTaxAddressWithTaxZone address zone =
        match (address, zone) with
            | (Country cc, OneCountry occ) ->
                cc = occ
            | (Country cc, SeveralCountries ccList) ->
                List.contains cc ccList
            | (Country cc, OneCountryOneState {CountryCode = occ; State = _}) ->
                cc = occ
            | (Country cc, OneCountryOneStateWithZips {CountryCode = occ; State = state; ZipCodes = _}) ->
                cc = occ
            | (CountryAndState {CountryCode = cc; State = state}, OneCountry occ) ->
                cc = occ
            | (CountryAndState {CountryCode = cc; State = state}, SeveralCountries ccList) ->
                List.contains cc ccList
            | (CountryAndState {CountryCode = cc; State = state}, OneCountryOneStateWithZips {CountryCode = occ; State = ostate; ZipCodes = _}) ->
                cc = occ && state = ostate
            | (CountryAndStateAndZip {CountryCode = cc; State = state; ZipCode = zip}, OneCountry occ) ->
                cc = occ
            | (CountryAndStateAndZip {CountryCode = cc; State = state; ZipCode = zip}, SeveralCountries ccList) ->
                List.contains cc ccList
            | (CountryAndStateAndZip {CountryCode = cc; State = state; ZipCode = zip}, OneCountryOneStateWithZips {CountryCode = occ; State = ostate; ZipCodes = zipList}) ->
                cc = occ && state = ostate && List.contains zip zipList
            | (_, RestOfWorld) -> true
            | (_, _) -> false

    let findBestTaxMatch : FindBestTaxMatch =
        fun taxInfoList taxQuery ->
            None


// public api for the TaxLib
module TaxLibApi =

    // DTO: Data Transfer Object

    type TaxInfoDTO = {
        Id: int
        Rate: decimal
    }

    type TaxQueryDTO = {
        CountryCode: string
        State: string option
        ZipCode: string option
        ProductInfo: string list
        CustomerType: string
    }
