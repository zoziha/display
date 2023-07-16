program example_display_character

    use display_module, only: display
    implicit none
    integer :: i

    call display('.true.', '.true.:')
    call display('inline output only for scalar', 'inline output:', inline=.true.) ! inline output only for scalar
    call display('.true.', '.true.:', unit=6) ! default format is 'i0'
    call display([('.true.', i=1, 10)], '1:10:')
    call display([('.true.', i=1, 10)], header='1:10:', brief=.false.)
    call display(reshape([('.true.', i=1, 25)], [5, 5]), '5x5:')
    call display(reshape([('.true.', i=1, 36)], [6, 6]), '6x6:', .true.)
    call display(reshape([('.false.', i=1, -35, -1)], [6, 6]), '6x6:', .false.)

end program example_display_character
! [scalar] .true.:
! '.true.'
! [scalar] inline output: 'inline output only for scalar'
! [scalar] .true.:
! '.true.'
! [vector: 10] 1:10:
! '.true.', '.true.', '.true.', ... '.true.'
! [vector: 10] 1:10:
! '.true.', '.true.', '.true.', '.true.', '.true.', '.true.', '.true.', '.true.', '.true.', '.true.'
! [matrix: 5*5] 5x5:
! '.true.', '.true.', '.true.', '.true.', '.true.';
! '.true.', '.true.', '.true.', '.true.', '.true.';
! '.true.', '.true.', '.true.', '.true.', '.true.';
! '.true.', '.true.', '.true.', '.true.', '.true.';
! '.true.', '.true.', '.true.', '.true.', '.true.'
! [matrix: 6*6] 6x6:
! '.true.', '.true.', '.true.', ... '.true.';
! '.true.', '.true.', '.true.', ... '.true.';
! '.true.', '.true.', '.true.', ... '.true.';
!  :
! '.true.', '.true.', '.true.', ... '.true.'
! [matrix: 6*6] 6x6:
! '.false.', '.false.', '.false.', '.false.', '.false.', '.false.';
! '.false.', '.false.', '.false.', '.false.', '.false.', '.false.';
! '.false.', '.false.', '.false.', '.false.', '.false.', '.false.';
! '.false.', '.false.', '.false.', '.false.', '.false.', '.false.';
! '.false.', '.false.', '.false.', '.false.', '.false.', '.false.';
! '.false.', '.false.', '.false.', '.false.', '.false.', '.false.'
