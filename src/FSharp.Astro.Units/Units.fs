namespace FSharp.Astro.Units

// Every unit of measure this library speaks. Open FSharp.Astro.Units and write 3.2<pc>, 0.5<arcsec>
// or 1.4e-3<Jy>.
//
// F# erases units at compile time, so a float<pc> is a plain float in the compiled assembly and
// costs nothing at runtime. A unit also carries no scale: pc and ly are unrelated types until a
// conversion factor such as Length.metersPerParsec relates them. The quantity modules (Length,
// Angle, Time, ...) hold those factors.
//
// Names follow the SI symbols and IAU recommendations, with ASCII spellings where a symbol needs
// one: um for micrometre, uas for microarcsecond, AA for angstrom.

// ---------------------------------------------------------------------------------------------
// SI units, re-exported from FSharp.Core so one `open FSharp.Astro.Units` is enough. Each is the
// same measure as its namesake in FSharp.Data.UnitSystems.SI.UnitSymbols, so values flow between
// this library and any other F# code that uses the standard SI units.
// ---------------------------------------------------------------------------------------------

/// Meter, the SI unit of length.
[<Measure>]
type m = Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols.m

/// Kilogram, the SI unit of mass.
[<Measure>]
type kg = Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols.kg

/// Second, the SI unit of time.
[<Measure>]
type s = Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols.s

/// Kelvin, the SI unit of thermodynamic temperature.
[<Measure>]
type K = Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols.K

/// Hertz, the SI unit of frequency. Identical to 1/s.
[<Measure>]
type Hz = Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols.Hz

/// Newton, the SI unit of force. Identical to kg m/s^2.
[<Measure>]
type N = Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols.N

/// Pascal, the SI unit of pressure. Identical to N/m^2.
[<Measure>]
type Pa = Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols.Pa

/// Joule, the SI unit of energy. Identical to N m.
[<Measure>]
type J = Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols.J

/// Watt, the SI unit of power. Identical to J/s.
[<Measure>]
type W = Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols.W

// ---------------------------------------------------------------------------------------------
// Angle. The SI treats the radian as dimensionless, but keeping it as a unit stops an angle from
// being mistaken for a plain number or an angle in degrees for one in radians.
// ---------------------------------------------------------------------------------------------

/// Radian.
[<Measure>]
type rad

/// Degree, pi/180 rad.
[<Measure>]
type deg

/// Arcminute, 1/60 degree.
[<Measure>]
type arcmin

/// Arcsecond, 1/3600 degree.
[<Measure>]
type arcsec

/// Milliarcsecond, the working unit of parallax and proper motion.
[<Measure>]
type mas

/// Microarcsecond.
[<Measure>]
type uas

/// Hour of right ascension, 15 degrees.
[<Measure>]
type hourangle

/// Steradian, the unit of solid angle. Identical to rad^2.
[<Measure>]
type sr = rad^2

// ---------------------------------------------------------------------------------------------
// Length.
// ---------------------------------------------------------------------------------------------

/// Kilometer.
[<Measure>]
type km

/// Centimeter.
[<Measure>]
type cm

/// Millimeter.
[<Measure>]
type mm

/// Micrometer, or micron.
[<Measure>]
type um

/// Nanometer.
[<Measure>]
type nm

/// Angstrom, 1e-10 m.
[<Measure>]
type angstrom

/// Angstrom, written the way astropy and the IAU style guide abbreviate it.
[<Measure>]
type AA = angstrom

/// Astronomical unit, exactly 149 597 870 700 m since IAU 2012 Resolution B2.
[<Measure>]
type au

/// Light year, the distance light travels in one Julian year.
[<Measure>]
type ly

/// Parsec, exactly 648000/pi au since IAU 2015 Resolution B2.
[<Measure>]
type pc

/// Kiloparsec.
[<Measure>]
type kpc

/// Megaparsec.
[<Measure>]
type Mpc

/// Gigaparsec.
[<Measure>]
type Gpc

/// Nominal solar radius, IAU 2015 Resolution B3.
[<Measure>]
type Rsun

/// Nominal equatorial radius of the Earth, IAU 2015 Resolution B3.
[<Measure>]
type Rearth

/// Nominal equatorial radius of Jupiter, IAU 2015 Resolution B3.
[<Measure>]
type Rjup

// ---------------------------------------------------------------------------------------------
// Time.
// ---------------------------------------------------------------------------------------------

/// Millisecond.
[<Measure>]
type ms

/// Minute, 60 s.
[<Measure>]
type min

/// Hour, 3600 s.
[<Measure>]
type h

/// Day, 86400 s.
[<Measure>]
type d

/// Julian year, exactly 365.25 days. The year of light years, proper motions and ages.
[<Measure>]
type yr

/// Thousand Julian years.
[<Measure>]
type kyr

/// Million Julian years.
[<Measure>]
type Myr

/// Billion Julian years.
[<Measure>]
type Gyr

// ---------------------------------------------------------------------------------------------
// Mass.
// ---------------------------------------------------------------------------------------------

/// Gram.
[<Measure>]
type g

/// Solar mass, derived from the IAU 2015 nominal GM of the Sun and the CODATA value of G.
[<Measure>]
type Msun

/// Earth mass, derived from the IAU 2015 nominal GM of the Earth and the CODATA value of G.
[<Measure>]
type Mearth

/// Jupiter mass, derived from the IAU 2015 nominal GM of Jupiter and the CODATA value of G.
[<Measure>]
type Mjup

// ---------------------------------------------------------------------------------------------
// Energy and power.
// ---------------------------------------------------------------------------------------------

/// Erg, the cgs unit of energy, 1e-7 J.
[<Measure>]
type erg

/// Electron volt.
[<Measure>]
type eV

/// Kiloelectron volt.
[<Measure>]
type keV

/// Megaelectron volt.
[<Measure>]
type MeV

/// Gigaelectron volt.
[<Measure>]
type GeV

/// Nominal solar luminosity, IAU 2015 Resolution B3.
[<Measure>]
type Lsun

// ---------------------------------------------------------------------------------------------
// Frequency.
// ---------------------------------------------------------------------------------------------

/// Kilohertz.
[<Measure>]
type kHz

/// Megahertz.
[<Measure>]
type MHz

/// Gigahertz.
[<Measure>]
type GHz

/// Terahertz.
[<Measure>]
type THz

// ---------------------------------------------------------------------------------------------
// Spectral flux density.
// ---------------------------------------------------------------------------------------------

/// Jansky, 1e-26 W/(m^2 Hz).
[<Measure>]
type Jy

/// Millijansky.
[<Measure>]
type mJy

/// Microjansky.
[<Measure>]
type uJy

// ---------------------------------------------------------------------------------------------
// Logarithmic quantities.
// ---------------------------------------------------------------------------------------------

/// Astronomical magnitude. A difference of 5 mag is a factor of exactly 100 in flux.
[<Measure>]
type mag

// ---------------------------------------------------------------------------------------------
// Instants, as opposed to the durations above. A Julian Date is a point on a timeline, so the
// factors that relate these to each other are offsets rather than ratios and live in `Epoch`
// instead of a `convert`. Subtracting two instants gives a duration in days.
// ---------------------------------------------------------------------------------------------

/// Julian Date, days elapsed since noon on 1 January 4713 BC in the proleptic Julian calendar.
[<Measure>]
type jd

/// Modified Julian Date, JD - 2400000.5. Zero at midnight on 17 November 1858.
[<Measure>]
type mjd

/// Julian epoch in years, the J of J2000.0. One jyear is exactly 365.25 days.
[<Measure>]
type jyear

/// Besselian epoch in years, the B of B1950.0. One byear is a tropical year at B1900.0.
[<Measure>]
type byear
