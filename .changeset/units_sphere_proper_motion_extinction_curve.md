---
default: minor
---

# Spherical geometry, proper motion, extinction curves, apparent sidereal time and SDSS bands

More of `FSharp.Astro.Units`.

`Spherical` is geometry on the celestial sphere: angular separation, position angle, the point a
given offset away from another, midpoints, and the solid angle of a cap or a coordinate band.
`separation` uses the Vincenty formula, which keeps its precision at an arcsecond where the
spherical law of cosines loses half its digits, and at 180 degrees where the haversine form loses
them instead. `offset` transports along a great circle and so stays correct at the pole.

`ProperMotion` carries a source across the sky over an epoch gap, taking the catalogue
`mu_alpha cos delta` convention and transporting along the great circle the motion points down.
Also total motion, position angle, and tangential velocity, out of which the usual 4.74 falls.

`Extinction` is a new module holding everything about dust. The scalar helpers move here from
`Photometry`, which keeps the systems and zero points, and they are joined by `ExtinctionLaw` and
the Cardelli, Clayton and Mathis 1989 mean Galactic curve, so the extinction in V now gives the
extinction at any wavelength from 0.1 to 3.3 micrometres.

`Epoch` gains the mean obliquity, the equation of the equinoxes and Greenwich and local apparent
sidereal time.

`Photometry.Bands` gains the SDSS `u` to `z` bands in `sdssAll`.

## Breaking

The extinction helpers move from `Photometry` to `Extinction` under the same names:
`ofColumn`, `ofColourExcess`, `colourExcess`, `colourExcessOfColours`, `extinguish`, `deredden`,
`transmission`, `ofOpticalDepth`, `opticalDepth` and `rvDiffuse`. `Photometry.absolute` and
`Photometry.apparent`, which take an extinction but are about magnitudes, stay where they are.
