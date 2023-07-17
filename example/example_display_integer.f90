program example_display_integer

    use display_module, only: display
    implicit none
    integer :: i

    call display(2)
    call display(2, inline=.true.)
    call display(1, '1:')
    call display(1, '1:', inline=.true.) ! inline output only for scalar
    call display(1, '1:', format='sp,i0', unit=6) ! default format is 'i0'
    call display([(i, i=1, 10)], '1:10:')
    call display([(i, i=1, 10)], header='1:10:', brief=.false.)
    call display(reshape([(i, i=1, 25)], [5, 5]), '5x5:')
    call display(reshape([(i, i=1, 36)], [6, 6]), '6x6:', .true.)
    call display(reshape([(i, i=1, -35, -1)], [6, 6]), '6x6:', .false., format='sp,i3')

end program example_display_integer
! [scalar] 1:
! 1
! [scalar] 1: 1
! [scalar] 1:
! +1
! [vector: 10] 1:10:
! 1, 2, 3, ... 10
! [vector: 10] 1:10:
! 1, 2, 3, 4, 5, 6, 7, 8, 9, 10
! [matrix: 5*5] 5x5:
! 1, 6, 11, 16, 21;
! 2, 7, 12, 17, 22;
! 3, 8, 13, 18, 23;
! 4, 9, 14, 19, 24;
! 5, 10, 15, 20, 25
! [matrix: 6*6] 6x6:
! 1, 7, 13, ... 31;
! 2, 8, 14, ... 32;
! 3, 9, 15, ... 33;
!  :
! 6, 12, 18, ... 36
! [matrix: 6*6] 6x6:
!  +1,  -5, -11, -17, -23, -29;
!  +0,  -6, -12, -18, -24, -30;
!  -1,  -7, -13, -19, -25, -31;
!  -2,  -8, -14, -20, -26, -32;
!  -3,  -9, -15, -21, -27, -33;
!  -4, -10, -16, -22, -28, -34
