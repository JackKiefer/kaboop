module kaboop_funcs
implicit none
contains

  function getArgs()
    character(len=32) :: arg
    character(len=32), allocatable, dimension(:) :: getArgs
    integer :: argc 
    integer :: i

    argc = command_argument_count()
    allocate(getArgs(argc))
    do i=1,argc
      call get_command_argument(i, arg)
      if (len_trim(arg) == 0) exit
      getArgs(i) = arg
    end do
  end function

  pure function doOp(params, i) result(newParams)
    character(len=32), allocatable, dimension(:), intent(in) :: params
    character(len=32), allocatable, dimension(:) :: newParams
    integer, intent(in) :: i
    double precision :: res, a, b

    allocate(newParams(size(params) - 2))

    read(params(i-2),*) a
    read(params(i-1),*) b

    select case (params(i))
      case ("+")
        res = a + b
      case ("-")
        res = a - b
      case ("/")
        res = a / b
      case ("*")
        res = a * b
      case ("^")
        res = a ** b
      case ("**")
        res = a ** b
    end select

    if (i > 3) then
      newParams(1:(i - 3)) = params(1:(i - 3))
    endif

    write(newParams(i-2), *) res
    newParams(i-1:(size(newParams))) = params((i+1):(size(params)))
  end function

  pure recursive function evaluate(params, ops) result(res)
    character(len=32), allocatable, dimension(:), intent(in) :: params
    character(len=32) :: res
    character(len=32), dimension(6), intent(in) :: ops
    integer :: i

    if (size(params) == 1) then
      res = params(1)
    else
      do i=1,size(params)
        if (any(ops == params(i))) then
          res = evaluate(doOp(params, i), ops)
          exit
        endif
      end do
    endif

  end function

end module

program kaboop
use kaboop_funcs
implicit none

  character(len=32), dimension(6) :: ops
  ops(1) = "+"
  ops(2) = "-"
  ops(3) = "/"
  ops(4) = "*"
  ops(5) = "^"
  ops(6) = "**"

  print *, evaluate(getArgs(), ops)
 
end program
