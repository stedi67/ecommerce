# fsharp

some sample code to try out domain modeling with fsharp (on dotnet)

## ECommerce

Initial setup of the project structure (including test setup with expecto)

  * `dotnet new -i Expecto.Template::*`
  * `dotnet new sln -o ECommerce`
  * `dotnet new classlib -lang F# -o src/TaxLib`
  * `dotnet new expecto -n ECommerceTests -o tests/ECommerceTests`
  * `dotnet add src/TaxLib/TaxLib.fsproj`
  * `dotnet add tests/ECommerceTests/ECommerceTests.fsproj reference src/TaxLib/TaxLib.fsproj`
