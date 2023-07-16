program example_display_logical

    use display_module, only: display
    implicit none
    integer :: i

    call display(.true., '.true.:')
    call display(.true., '.true.:', inline=.true.) ! inline output only for scalar
    call display(.true., '.true.:', unit=6) ! default format is 'i0'
    call display([(.true., i=1, 10)], '1:10:')
    call display([(.true., i=1, 10)], header='1:10:', brief=.false.)
    call display(reshape([(.true., i=1, 25)], [5, 5]), '5x5:')
    call display(reshape([(.true., i=1, 36)], [6, 6]), '6x6:', .true.)
    call display(reshape([(.false., i=1, -35, -1)], [6, 6]), '6x6:', .false.)

end program example_display_logical
! [scalar] .true.:
! T
! [scalar] .true.: T
! [scalar] .true.:
! T
! [vector: 10] 1:10:
! T, T, T, ... T
! [vector: 10] 1:10:
! T, T, T, T, T, T, T, T, T, T
! [matrix: 5*5] 5x5:
! T, T, T, T, T;
! T, T, T, T, T;
! T, T, T, T, T;
! T, T, T, T, T;
! T, T, T, T, T
! [matrix: 6*6] 6x6:
! T, T, T, ... T;
! T, T, T, ... T;
! T, T, T, ... T;
!  :
! T, T, T, ... T
! [matrix: 6*6] 6x6:
! F, F, F, F, F, F;
! F, F, F, F, F, F;
! F, F, F, F, F, F;
! F, F, F, F, F, F;
! F, F, F, F, F, F;
! F, F, F, F, F, F
