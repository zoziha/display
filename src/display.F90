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
        module procedure :: display_0dr, display_1dr, display_2dr
        module procedure :: display_0di, display_1di, display_2di
        module procedure :: display_0dl, display_1dl, display_2dl
        module procedure :: display_0ds, display_1ds, display_2ds
        module procedure :: display_0dc, display_1dc, display_2dc
    end interface display

    !> Private routine: Convert scalar to string
    interface to_string
        module procedure :: to_string_real_kind
        module procedure :: to_string_integer_kind
        module procedure :: to_string_logical_kind
        module procedure :: to_string_character_kind
        module procedure :: to_string_complex_kind
    end interface to_string

    !> Private routine: Convert vector to string
    interface vector_string
        module procedure :: real_vector_string
        module procedure :: integer_vector_string
        module procedure :: logical_vector_string
        module procedure :: character_vector_string
        module procedure :: complex_vector_string
    end interface vector_string

    character(*), parameter :: default_real_format = 'es10.3'  !! default real format string
    character(*), parameter :: default_int_format = 'i0'  !! default integer format string
    integer, parameter :: default_length = 64  !! default string length

contains

    !> Output floating point scalar on screen
    subroutine display_0dr(re, header, brief, format, unit, inline)
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
            format_ = default_real_format
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
            if (inline_) then
                str = header//' '
            else
                str = header
            end if
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

    end subroutine display_0dr

    !> Output integer scalar on screen
    subroutine display_0di(re, header, brief, format, unit, inline)
        integer, intent(in) :: re  !! scalar
        character(*), intent(in), optional :: header  !! header string
        logical, intent(in), optional :: brief  !! brief output, default is `.true.`
        character(*), intent(in), optional :: format  !! format string, default is `i0`
        integer, intent(in), optional :: unit  !! output unit, default is `output_unit`
        logical, intent(in), optional :: inline  !! inline output, default is `.false.`
        logical :: brief_, inline_
        character(:), allocatable :: str
        character(:), allocatable :: format_
        integer :: unit_

        if (present(format)) then
            format_ = format
        else
            format_ = default_int_format
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
            if (inline_) then
                str = header//' '
            else
                str = header
            end if
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

    end subroutine display_0di

    !> Output logical scalar on screen
    subroutine display_0dl(re, header, brief, unit, inline)
        logical, intent(in) :: re  !! scalar
        character(*), intent(in), optional :: header  !! header string
        logical, intent(in), optional :: brief  !! brief output, default is `.true.`
        integer, intent(in), optional :: unit  !! output unit, default is `output_unit`
        logical, intent(in), optional :: inline  !! inline output, default is `.false.`
        logical :: brief_, inline_
        character(:), allocatable :: str
        integer :: unit_

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
            if (inline_) then
                str = header//' '
            else
                str = header
            end if
        else
            str = ''
        end if

        if (present(unit)) then
            unit_ = unit
        else
            unit_ = output_unit
        end if

        if (.not.inline_) str = str//nl
        str = '[scalar] '//str//to_string(re)

        write (unit_, '(a)') str

    end subroutine display_0dl

    !> Output character scalar on screen
    subroutine display_0ds(re, header, brief, unit, inline)
        character(*), intent(in) :: re  !! scalar
        character(*), intent(in), optional :: header  !! header string
        logical, intent(in), optional :: brief  !! brief output, default is `.true.`
        integer, intent(in), optional :: unit  !! output unit, default is `output_unit`
        logical, intent(in), optional :: inline  !! inline output, default is `.false.`
        logical :: brief_, inline_
        character(:), allocatable :: str
        integer :: unit_

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
            if (inline_) then
                str = header//' '
            else
                str = header
            end if
        else
            str = ''
        end if

        if (present(unit)) then
            unit_ = unit
        else
            unit_ = output_unit
        end if

        if (.not.inline_) str = str//nl
        str = '[scalar] '//str//to_string(re)

        write (unit_, '(a)') str

    end subroutine display_0ds

    !> Output complex scalar on screen
    subroutine display_0dc(re, header, brief, format, unit, inline)
        complex(kind=rk), intent(in) :: re  !! scalar
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
            format_ = default_real_format
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
            if (inline_) then
                str = header//' '
            else
                str = header
            end if
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

    end subroutine display_0dc

    !> Output floating point vector on screen
    subroutine display_1dr(re, header, brief, format, unit)
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
            format_ = default_real_format
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

    end subroutine display_1dr

    !> Output integer vector on screen
    subroutine display_1di(re, header, brief, format, unit)
        integer, intent(in) :: re(:)  !! vector
        character(*), intent(in), optional :: header  !! header string
        logical, intent(in), optional :: brief  !! brief output, default is `.true.`
        character(*), intent(in), optional :: format  !! format string, default is `i0`
        integer, intent(in), optional :: unit  !! output unit, default is `output_unit`
        logical :: brief_
        character(:), allocatable :: str
        character(:), allocatable :: format_
        integer :: unit_

        if (present(format)) then
            format_ = format
        else
            format_ = default_int_format
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

    end subroutine display_1di

    !> Output logical vector on screen
    subroutine display_1dl(re, header, brief, unit)
        logical, intent(in) :: re(:)  !! vector
        character(*), intent(in), optional :: header  !! header string
        logical, intent(in), optional :: brief  !! brief output, default is `.true.`
        integer, intent(in), optional :: unit  !! output unit, default is `output_unit`
        logical :: brief_
        character(:), allocatable :: str
        integer :: unit_

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
        call vector_string(re, brief_, str)

        write (unit_, '(a)') str

    end subroutine display_1dl

    !> Output character vector on screen
    subroutine display_1ds(re, header, brief, unit)
        character(*), intent(in) :: re(:)  !! vector
        character(*), intent(in), optional :: header  !! header string
        logical, intent(in), optional :: brief  !! brief output, default is `.true.`
        integer, intent(in), optional :: unit  !! output unit, default is `output_unit`
        logical :: brief_
        character(:), allocatable :: str
        integer :: unit_

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
        call vector_string(re, brief_, str)

        write (unit_, '(a)') str

    end subroutine display_1ds

    !> Output complex vector on screen
    subroutine display_1dc(re, header, brief, format, unit)
        complex(kind=rk), intent(in) :: re(:)  !! vector
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
            format_ = default_real_format
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

    end subroutine display_1dc

    !> Output floating point matrix on screen
    subroutine display_2dr(re, header, brief, format, unit)
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
            format_ = default_real_format
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

    end subroutine display_2dr

    !> Output integer matrix on screen
    subroutine display_2di(re, header, brief, format, unit)
        integer, intent(in) :: re(:, :)  !! matrix
        character(*), intent(in), optional :: header  !! header string
        logical, intent(in), optional :: brief  !! brief output, default is `.true.`
        character(*), intent(in), optional :: format  !! format string, default is `i0`
        integer, intent(in), optional :: unit  !! output unit, default is `output_unit`
        logical :: brief_
        character(:), allocatable :: str
        character(:), allocatable :: format_
        integer :: unit_
        integer :: i

        if (present(format)) then
            format_ = format
        else
            format_ = default_int_format
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

    end subroutine display_2di

    !> Output logical matrix on screen
    subroutine display_2dl(re, header, brief, unit)
        logical, intent(in) :: re(:, :)  !! matrix
        character(*), intent(in), optional :: header  !! header string
        logical, intent(in), optional :: brief  !! brief output, default is `.true.`
        integer, intent(in), optional :: unit  !! output unit, default is `output_unit`
        logical :: brief_
        character(:), allocatable :: str
        integer :: unit_
        integer :: i

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
                call vector_string(re(i, :), brief_, str)
                str = str//";"//nl
            end do
            str = str//' : '//nl
            call vector_string(re(size(re, 1), :), brief_, str)
        else
            do i = 1, size(re, 1) - 1
                call vector_string(re(i, :), brief_, str)
                str = str//";"//nl
            end do
            call vector_string(re(size(re, 1), :), brief_, str)
        end if

        write (unit_, '(a)') str

    end subroutine display_2dl

    !> Output character matrix on screen
    subroutine display_2ds(re, header, brief, unit)
        character(*), intent(in) :: re(:, :)  !! matrix
        character(*), intent(in), optional :: header  !! header string
        logical, intent(in), optional :: brief  !! brief output, default is `.true.`
        integer, intent(in), optional :: unit  !! output unit, default is `output_unit`
        logical :: brief_
        character(:), allocatable :: str
        integer :: unit_
        integer :: i

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
                call vector_string(re(i, :), brief_, str)
                str = str//";"//nl
            end do
            str = str//' : '//nl
            call vector_string(re(size(re, 1), :), brief_, str)
        else
            do i = 1, size(re, 1) - 1
                call vector_string(re(i, :), brief_, str)
                str = str//";"//nl
            end do
            call vector_string(re(size(re, 1), :), brief_, str)
        end if

        write (unit_, '(a)') str

    end subroutine display_2ds

    !> Output complex matrix on screen
    subroutine display_2dc(re, header, brief, format, unit)
        complex(kind=rk), intent(in) :: re(:, :)  !! matrix
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
            format_ = default_real_format
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

    end subroutine display_2dc

    !> Private routine: Convert floating point vector to string
    subroutine real_vector_string(x, brief, format, string)
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

    end subroutine real_vector_string

    !> Private routine: Convert integer vector to string
    subroutine integer_vector_string(x, brief, format, string)
        integer, intent(in) :: x(:)
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

    end subroutine integer_vector_string

    !> Private routine: Convert logical vector to string
    subroutine logical_vector_string(x, brief, string)
        logical, intent(in) :: x(:)
        logical, intent(in) :: brief
        character(:), allocatable, intent(inout) :: string
        integer :: i

        if (brief .and. size(x) > 5) then
            string = string//to_string(x(1))//', '//to_string(x(2))//', '// &
                     to_string(x(3))//', ... '//to_string(x(size(x)))
        else
            string = string//to_string(x(1))
            do i = 2, size(x)
                string = string//', '//to_string(x(i))
            end do
        end if

    end subroutine logical_vector_string

    !> Private routine: Convert character vector to string
    subroutine character_vector_string(x, brief, string)
        character(*), intent(in) :: x(:)
        logical, intent(in) :: brief
        character(:), allocatable, intent(inout) :: string
        integer :: i

        if (brief .and. size(x) > 5) then
            string = string//to_string(x(1))//', '//to_string(x(2))//', '// &
                     to_string(x(3))//', ... '//to_string(x(size(x)))
        else
            string = string//to_string(x(1))
            do i = 2, size(x)
                string = string//', '//to_string(x(i))
            end do
        end if

    end subroutine character_vector_string

    !> Private routine: Convert complex vector to string
    subroutine complex_vector_string(x, brief, format, string)
        complex(kind=rk), intent(in) :: x(:)
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

    end subroutine complex_vector_string

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

    !> Private routine: Convert logical to string
    pure function to_string_logical_kind(logical) result(string)
        logical, intent(in) :: logical
        character(len=1) :: string

        if (logical) then
            string = 'T'
        else
            string = 'F'
        end if

    end function to_string_logical_kind

    !> Private routine: Convert character to string
    pure function to_string_character_kind(char) result(string)
        character(*), intent(in) :: char
        character(len=len(char) + 2) :: string

        string = "'"//char//"'"

    end function to_string_character_kind

    !> Private routine: Convert complex to string
    pure function to_string_complex_kind(complex, fmt) result(string)
        complex(kind=rk), intent(in) :: complex
        character(len=*), intent(in) :: fmt
        character(len=:), allocatable :: string
        character(len=default_length) :: s

        write (s, "(a1,"//fmt//",a1,"//fmt//",a1)") "(", complex%re, ",", complex%im, ")"
        string = trim(s)

    end function to_string_complex_kind

end module display_module
