# Simple Display

![Language](https://img.shields.io/badge/-Fortran-734f96?logo=fortran&logoColor=white)
[![license](https://img.shields.io/badge/License-MIT-pink)](LICENSE)

A simple display module for displaying scalar, vector, and matrix floating point values
on the screen in a general, formatted manner, often used for debugging code or syntax demonstration.

## Usage

Only FPM is supported, other build systems can copy source files (`./src/display.F90`) directly,
and `ifort` and `gfortran` compilers are tested.

To use `display` within your `fpm` project, add the following lines to your `fpm.toml` file:

```toml
[dependencies]
display = { git="https://github.com/zoziha/display" }
```

## Example

```sh
> fpm run --example --all  # run the example
```

```fortran
program example_display

    use display_module, only: display
    implicit none
    integer :: i

    call display(1.0, '1.0:')
    call display([real ::(i, i=1, 10)], '1:10:')
    call display([real ::(i, i=1, 10)], header='1:10:', brief=.false.)
    call display(reshape([real ::(i, i=1, 25)], [5, 5]), '5x5:')
    call display(reshape([real ::(i, i=1, 36)], [6, 6]), '6x6:', .true.)
    call display(reshape([real ::(i, i=1, -35, -1)], [6, 6]), '6x6:', .false.)

end program example_display
! [scalar] 1.0:
!  1.000E+00
! [vector: 10] 1:10:
!  1.000E+00,  2.000E+00,  3.000E+00, ...  1.000E+01
! [vector: 10] 1:10:
!  1.000E+00,  2.000E+00,  3.000E+00,  4.000E+00,  5.000E+00,  6.000E+00,  7.000E+00,  8.000E+00,  9.000E+00,  1.000E+01
! [matrix: 5*5] 5x5:
!  1.000E+00,  6.000E+00,  1.100E+01,  1.600E+01,  2.100E+01;
!  2.000E+00,  7.000E+00,  1.200E+01,  1.700E+01,  2.200E+01;
!  3.000E+00,  8.000E+00,  1.300E+01,  1.800E+01,  2.300E+01;
!  4.000E+00,  9.000E+00,  1.400E+01,  1.900E+01,  2.400E+01;
!  5.000E+00,  1.000E+01,  1.500E+01,  2.000E+01,  2.500E+01
! [matrix: 6*6] 6x6:
!  1.000E+00,  7.000E+00,  1.300E+01, ...  3.100E+01;
!  2.000E+00,  8.000E+00,  1.400E+01, ...  3.200E+01;
!  3.000E+00,  9.000E+00,  1.500E+01, ...  3.300E+01;
!  :
!  6.000E+00,  1.200E+01,  1.800E+01, ...  3.600E+01
! [matrix: 6*6] 6x6:
!  1.000E+00, -5.000E+00, -1.100E+01, -1.700E+01, -2.300E+01, -2.900E+01;
!  0.000E+00, -6.000E+00, -1.200E+01, -1.800E+01, -2.400E+01, -3.000E+01;
! -1.000E+00, -7.000E+00, -1.300E+01, -1.900E+01, -2.500E+01, -3.100E+01;
! -2.000E+00, -8.000E+00, -1.400E+01, -2.000E+01, -2.600E+01, -3.200E+01;
! -3.000E+00, -9.000E+00, -1.500E+01, -2.100E+01, -2.700E+01, -3.300E+01;
! -4.000E+00, -1.000E+01, -1.600E+01, -2.200E+01, -2.800E+01, -3.400E+01
```
