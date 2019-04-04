module TaxLib

type ISOCountryCode = ISOCountryCode of string

type ZipCode = ZipCode of string

type StateCounty = StateCounty of string

type TaxRegion =
  | DE
  | EU
  | NON_EU

type ProductType =
  | Hardware
  | Software
  | Book

type CustomerType =
  | B2C
  | B2B
  | IC

type TaxRate = TaxRate of decimal

type TaxZoneId = TaxZoneId string

type TaxZone = {
  TaxZoneId: TaxZoneId
  Region: TaxRegion
  CountryCode: ISOCountryCode
  State: StateCountry
  ZipCodes: ZipCode list
}

type Tax = {
  TaxZoneId: TaxZoneId
}
