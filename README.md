# fsharp

some sample code to try out domain modeling with fsharp (on dotnet).
Before starting, the current dotnet runtime needs to be installed. Follow [this](https://docs.microsoft.com/de-de/dotnet/fsharp/get-started/get-started-command-line) guide to set everything up.
This worked for me on ubuntu and mac.

## ECommerce

Initial setup of the project structure (including test setup with expecto)

  * `dotnet new -i Expecto.Template::*`
  * `dotnet new sln -o ECommerce`
  * `dotnet new classlib -lang F# -o src/TaxLib`
  * `dotnet new expecto -n ECommerceTests -o tests/ECommerceTests`
  * `dotnet add src/TaxLib/TaxLib.fsproj`
  * `dotnet add tests/ECommerceTests/ECommerceTests.fsproj reference src/TaxLib/TaxLib.fsproj`

  This is just a description on how the project was setup initially to have it documented somewhere.
  Within the `ECommerce` directory, it should be enough to run `dotnet build` to build everything.
  To run the tests, go to `ECommerce/tests/ECommerceTests` and run `dotnet run` to run the tests.
