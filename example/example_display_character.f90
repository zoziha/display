program example_display_character

    use display_module, only: display
    implicit none
    integer :: i

    call display('AA', '.true.:')
    call display('inline output only for scalar', 'inline output:', inline=.true.) ! inline output only for scalar
    call display('AB', '.true.:', unit=6) ! default format is 'i0'
    call display([('BA', i=1, 10)], '1:10:')
    call display([('BB', i=1, 10)], header='1:10:', brief=.false.)
    call display(reshape([('CA', i=1, 25)], [5, 5]), '5x5:')
    call display(reshape([('CB', i=1, 36)], [6, 6]), '6x6:', .true.)
    call display(reshape([('CC', i=1, -35, -1)], [6, 6]), '6x6:', .false.)

end program example_display_character
! [scalar] .true.:
! AA
! [scalar] inline output: inline output only for scalar
! [scalar] .true.:
! AB
! [vector: 10] 1:10:
! BA, BA, BA, ... BA
! [vector: 10] 1:10:
! BB, BB, BB, BB, BB, BB, BB, BB, BB, BB
! [matrix: 5*5] 5x5:
! CA, CA, CA, CA, CA;
! CA, CA, CA, CA, CA;
! CA, CA, CA, CA, CA;
! CA, CA, CA, CA, CA;
! CA, CA, CA, CA, CA
! [matrix: 6*6] 6x6:
! CB, CB, CB, ... CB;
! CB, CB, CB, ... CB;
! CB, CB, CB, ... CB;
!  :
! CB, CB, CB, ... CB
! [matrix: 6*6] 6x6:
! CC, CC, CC, CC, CC, CC;
! CC, CC, CC, CC, CC, CC;
! CC, CC, CC, CC, CC, CC;
! CC, CC, CC, CC, CC, CC;
! CC, CC, CC, CC, CC, CC;
! CC, CC, CC, CC, CC, CC
