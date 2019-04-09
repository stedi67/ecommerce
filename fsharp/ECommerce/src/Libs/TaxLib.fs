module TaxLib

type ISOCountryCode = ISOCountryCode of string

type ZipCode = ZipCode of string

type StateCountry = StateCountry of string

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
    ZipCode: ZipCode list
}

type TaxZone =
    | OneCountryOneStateWithZips of OneCountryOneStateWithZips
    | OneCountryOneState of OneCountryOneState
    | OneCountry of ISOCountryCode
    | SeveralCountries of ISOCountryCode list
    | RestOfWorld


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

let findBestTaxMatch : FindBestTaxMatch =
    fun taxInfoList taxQuery ->
        None
