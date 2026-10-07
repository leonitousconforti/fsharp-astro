module ConstantsTests

open Xunit
open FSharp.Astro.Units
open Helpers

[<Fact>]
let ``Stefan-Boltzmann constant matches CODATA 2018`` () =
    close 1e-9 5.670374419e-8<W / (m^2 K^4)> Constants.sigmaSB

[<Fact>]
let ``reduced Planck constant matches CODATA 2018`` () =
    close 1e-9 1.054571817e-34<J s> Constants.hbar

[<Fact>]
let ``Wien constant matches CODATA 2018`` () =
    close 1e-9 2.897771955e-3<m K> Constants.bWien

[<Fact>]
let ``solar mass matches the IAU 2015 nominal GM over CODATA 2018 G`` () =
    close 1e-9 1.988409870698051e30<kg> Constants.Msun

[<Fact>]
let ``Earth mass matches the IAU 2015 nominal GM over CODATA 2018 G`` () =
    close 1e-9 5.972167867791379e24<kg> Constants.Mearth

[<Fact>]
let ``Jupiter mass matches the IAU 2015 nominal GM over CODATA 2018 G`` () =
    close 1e-9 1.8981245973360505e27<kg> Constants.Mjup

[<Fact>]
let ``nominal solar values are self-consistent with a black body`` () =
    close 1e-3 Constants.Lsun (Luminosity.blackBody Constants.Rsun Constants.Tsun)

[<Fact>]
let ``nominal solar irradiance is the solar luminosity spread over the sphere at 1 au`` () =
    close 1e-3 Constants.solarIrradiance (Luminosity.flux Constants.Lsun (1.0<au> * Length.metersPerAu))
