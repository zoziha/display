!> Copyright (c) 2021~2023, ZUO Zhihua. Licensed by MIT.
module display_module

#ifdef REAL64
    use, intrinsic :: iso_fortran_env, only: rk => real64
#else
    use, intrinsic :: iso_fortran_env, only: rk => real32
#endif
    use, intrinsic :: iso_c_binding, only: nl => c_new_line
    use, intrinsic :: iso_fortran_env, only: output_unit
    implicit none

    private
    public :: display

    !> Output floating point array on screen, support scalar, vector, matrix
    !> @note Recommend format to use `fw.d`, `esw.d`, `ew.d`, `sp,fw.d` style, not `gw.d` style
    interface display
        module procedure :: display_0d
        module procedure :: display_1d
        module procedure :: display_2d
    end interface display

    !> Private routine: Convert floating or integer to string
    interface to_string
        module procedure :: to_string_real_kind
        module procedure :: to_string_integer_kind
    end interface to_string

    character(*), parameter :: default_format = 'es10.3'  !! default format string
    integer, parameter :: default_length = 64  !! default string length

contains

    !> Output floating point scalar on screen
    subroutine display_0d(re, header, brief, format, unit, inline)
        real(rk), intent(in) :: re  !! scalar
        character(*), intent(in), optional :: header  !! header string
        logical, intent(in), optional :: brief  !! brief output, default is `.true.`
        character(*), intent(in), optional :: format  !! format string, default is `es10.3`
        integer, intent(in), optional :: unit  !! output unit, default is `output_unit`
        logical, intent(in), optional :: inline  !! inline output, default is `.false.`
        logical :: brief_, inline_
        character(:), allocatable :: str
        character(:), allocatable :: format_
        integer :: unit_

        if (present(format)) then
            format_ = format
        else
            format_ = default_format
        end if

        if (present(brief)) then
            brief_ = brief
        else
            brief_ = .true.
        end if

        if (present(inline)) then
            inline_ = inline
        else
            inline_ = .false.
        end if

        if (present(header)) then
            str = header
        else
            str = ''
        end if

        if (present(unit)) then
            unit_ = unit
        else
            unit_ = output_unit
        end if

        if (.not.inline_) str = str//nl
        str = '[scalar] '//str//to_string(re, format_)

        write (unit_, '(a)') str

    end subroutine display_0d

    !> Output floating point vector on screen
    subroutine display_1d(re, header, brief, format, unit)
        real(rk), intent(in) :: re(:)  !! vector
        character(*), intent(in), optional :: header  !! header string
        logical, intent(in), optional :: brief  !! brief output, default is `.true.`
        character(*), intent(in), optional :: format  !! format string, default is `es10.3`
        integer, intent(in), optional :: unit  !! output unit, default is `output_unit`
        logical :: brief_
        character(:), allocatable :: str
        character(:), allocatable :: format_
        integer :: unit_

        if (present(format)) then
            format_ = format
        else
            format_ = default_format
        end if

        if (present(brief)) then
            brief_ = brief
        else
            brief_ = .true.
        end if

        if (present(header)) then
            str = header
        else
            str = ''
        end if

        if (present(unit)) then
            unit_ = unit
        else
            unit_ = output_unit
        end if

        str = '[vector: '//to_string(size(re), 'i0')//'] '//str//nl
        call vector_string(re, brief_, format_, str)

        write (unit_, '(a)') str

    end subroutine display_1d

    !> Output floating point matrix on screen
    subroutine display_2d(re, header, brief, format, unit)
        real(rk), intent(in) :: re(:, :)  !! matrix
        character(*), intent(in), optional :: header  !! header string
        logical, intent(in), optional :: brief  !! brief output, default is `.true.`
        character(*), intent(in), optional :: format  !! format string, default is `es10.3`
        integer, intent(in), optional :: unit  !! output unit, default is `output_unit`
        logical :: brief_
        character(:), allocatable :: str
        character(:), allocatable :: format_
        integer :: unit_
        integer :: i

        if (present(format)) then
            format_ = format
        else
            format_ = default_format
        end if

        if (present(brief)) then
            brief_ = brief
        else
            brief_ = .true.
        end if

        if (present(header)) then
            str = header
        else
            str = ''
        end if

        if (present(unit)) then
            unit_ = unit
        else
            unit_ = output_unit
        end if

        str = '[matrix: '//to_string(size(re, 1), 'i0')//'*'//to_string(size(re, 2), 'i0')//'] '//str//nl
        if (brief_ .and. size(re, 1) > 5) then
            do i = 1, 3
                call vector_string(re(i, :), brief_, format_, str)
                str = str//";"//nl
            end do
            str = str//' : '//nl
            call vector_string(re(size(re, 1), :), brief_, format_, str)
        else
            do i = 1, size(re, 1) - 1
                call vector_string(re(i, :), brief_, format_, str)
                str = str//";"//nl
            end do
            call vector_string(re(size(re, 1), :), brief_, format_, str)
        end if

        write (unit_, '(a)') str

    end subroutine display_2d

    !> Private routine: Convert floating point vector to string
    subroutine vector_string(x, brief, format, string)
        real(rk), intent(in) :: x(:)
        logical, intent(in) :: brief
        character(*), intent(in) :: format
        character(:), allocatable, intent(inout) :: string
        integer :: i

        if (brief .and. size(x) > 5) then
            string = string//to_string(x(1), format)//', '//to_string(x(2), format)//', '// &
                     to_string(x(3), format)//', ... '//to_string(x(size(x)), format)
        else
            string = string//to_string(x(1), format)
            do i = 2, size(x)
                string = string//', '//to_string(x(i), format)
            end do
        end if

    end subroutine vector_string

    !> Private routine: Convert floating point to string
    pure function to_string_real_kind(real, fmt) result(string)
        real(kind=rk), intent(in) :: real
        character(len=*), intent(in) :: fmt
        character(len=:), allocatable :: string
        character(len=default_length) :: s

        write (s, "("//fmt//")") real
        string = trim(s)

    end function to_string_real_kind

    !> Private routine: Convert integer to string
    pure function to_string_integer_kind(integer, fmt) result(string)
        integer, intent(in) :: integer
        character(len=*), intent(in) :: fmt
        character(len=:), allocatable :: string
        character(len=default_length) :: s

        write (s, "("//fmt//")") integer
        string = trim(s)

    end function to_string_integer_kind

end module display_module
