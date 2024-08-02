# sfcurve: 2x2, 3x3 and nxn Space-Filling Curves

![](https://github.com/user-attachments/assets/7e0e14e7-1300-421f-8ffe-113b80caee97)


This package provides a way to encode all possible forms of 2x2 and 3x3
space-filling curves. For example, the following eight forms correspond to the
2x2 curve on level 3 and with `R(0)` (going-right base pattern with rotation
of 0 degree) as the seed.

<img src="https://github.com/user-attachments/assets/82b56013-8e9e-45f6-b77a-0875769c6369" width=700 />

It also supports nxn curves expanded from specific level-1 units.

## Install


```r
devtools::install_github("jokergoo/sfcurve")
```

## Usage

Hilbert curve (2x2):

```
> sfc_hilbert("I", "111")
An sfc_hilbert object.
  Increase mode: 2 x 2
  Level: 3
  Expansion rule: Hilbert

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
> sfc_peano("I", "111")
An sfc_peano object.
  Increase mode: 3 x 3
  Level: 3
  Expansion rule: Peano

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
> sfc_meander("I", "111")
An sfc_meander object.
  Increase mode: 3 x 3
  Level: 3
  Expansion rule: Meander

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
p2 = sfc_hilbert(p, "1111")
plot(p2)
```

<img src="https://github.com/user-attachments/assets/f1144f7f-282f-4988-aafd-9f712dd3ed2d" width=500 />


## License

MIT @ Zuguang Gu
