module Tests

open Expecto
open ECommerce.TaxAddress

[<Tests>]
let tests =
    testList "Tax Tests" [
        test "de" {
            let res = createTaxAddress "de" "" ""
            let expected = Ok {Country=ISOCountryCode "de"; State=None; Zip=None}
            Expect.equal res expected "create german tax address"
        }

    ]

[<Tests>]
let moreTests =
    testList "More" [
        test "d" {
            let res = createTaxAddress "d" "" ""
            let expected = Error "Country Code needs to be a 2 char ISO country code"
            Expect.equal res expected "create address with non ISO CC"
        }
    ]
