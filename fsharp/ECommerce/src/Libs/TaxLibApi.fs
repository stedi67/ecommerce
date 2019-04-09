module TaxLibApi

// DTO: Data Transfer Object

type TaxAddressDTO = {
    CountryCode: string
    State: string option
    ZipCode: string option
}

type TaxInfoDTO = {
    Id: int
    Rate: decimal
}
