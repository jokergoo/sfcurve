# sfcurve: 2x2, 3x3 and nxn Space-Filling Curves

[![CRAN](https://www.r-pkg.org/badges/version/sfcurve)](https://cran.r-project.org/web/packages/sfcurve/index.html)
[![CRAN](https://cranlogs.r-pkg.org/badges/grand-total/sfcurve)](https://cran.r-project.org/web/packages/sfcurve/index.html)


![](https://github.com/user-attachments/assets/7e0e14e7-1300-421f-8ffe-113b80caee97)


This package provides a way to encode all possible forms of 2x2 and 3x3
space-filling curves. For example, the following eight forms correspond to the
2x2 curve on level 3 and with `R(0)` (bottom-in right-out base pattern with rotation
of 0 degree) as the seed.

<img src="https://github.com/user-attachments/assets/82b56013-8e9e-45f6-b77a-0875769c6369" width=700 />

It also supports nxn curves expanded from any valid level-1 unit.

## Install

```r
install.packages("sfcurve")
```

or the devel version:

```r
devtools::install_github("jokergoo/sfcurve")
```

## Usage

Hilbert curve (2x2):

```
> sfc_2x2("I", "111")
An sfc_2x2 object.
  Increase mode: 2 x 2
  Level: 3
  Expansion rule: 2x2

A sequence of 64 base patterns.
  R(0)L(270)L(0)R(90)     I(0)R(0)R(270)L(180)
  L(270)R(0)R(270)I(180)  R(180)L(90)L(180)I(270)
  .... other 4 lines ....
  I(90)L(90)L(180)R(270)  I(180)R(180)R(90)L(0)
  L(90)R(180)R(90)I(0)    R(0)L(270)L(0)R(90)

Seed: A sequence of 1 base pattern.
  I(0)
```

Peano curve (3x3):

```
> sfc_3x3_peano("I", "111")
An sfc_3x3_peano object.
  Increase mode: 3 x 3
  Level: 3
  Expansion rule: 3x3 Peano

A sequence of 729 base patterns.
  I(0)J(0)R(0)R(270)  I(180)L(180)L(270)J(0)
  I(0)J(0)I(0)L(0)    L(90)J(180)R(180)R(90)
  .... other 88 lines ....
  I(0)J(0)R(0)R(270)  I(180)L(180)L(270)J(0)
  I(0)

Seed: A sequence of 1 base pattern.
  I(0)
```

Meander curve (3x3):

```
> sfc_3x3_meander("I", "111")
An sfc_3x3_meander object.
  Increase mode: 3 x 3
  Level: 3
  Expansion rule: 3x3 Meander

A sequence of 729 base patterns.
  R(0)I(270)L(270)I(0)  L(0)L(90)R(180)R(90)
  I(0)R(0)I(270)L(270)  I(0)L(0)L(90)R(180)
  .... other 88 lines ....
  R(0)I(270)L(270)I(0)  L(0)L(90)R(180)R(90)
  I(0)

Seed: A sequence of 1 base pattern.
  I(0)
```

It also allows using a sequence as the seed:


```
p = sfc_seed("LLLILILIILIILIIILIIILIIII")
p2 = sfc_2x2(p, "1111")
plot(p2)
```

<img src="https://github.com/user-attachments/assets/f1144f7f-282f-4988-aafd-9f712dd3ed2d" width=500 />

For more comprehensive introduction of the theory and the package, please refer to the vignettes.

## License

MIT @ Zuguang Gu
