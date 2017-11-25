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

  pure function factorial(n)
    double precision :: factorial
    double precision, intent(in) :: n 
    integer :: x, i

    x = int(n)

    if (x < 0) then
      factorial = 0
    else if (x == 0) then
      factorial = 1
    else if (x == 1) then
      factorial = 1
    else
      do i = (x-1), 1, -1
        x = x * i
      end do
      factorial = x
    endif
  end function

  pure function doBinaryOp(params, i) result(newParams)
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

  pure function doUnaryOp(params, i) result(newParams)
    character(len=32), allocatable, dimension(:), intent(in) :: params
    character(len=32), allocatable, dimension(:) :: newParams
    integer, intent(in) :: i
    double precision :: res, n
    
    allocate(newParams(size(params) - 1))

    read(params(i-1),*) n

    select case (params(i))
      case ("factorial")
        res = factorial(n)
    end select

    if (i > 2) then
      newParams(1:(i-2)) = params(1:(i - 2))
    endif

    write(newParams(i-1), *) res
    newParams(i-1:(size(newParams))) = params((i+1):(size(params)))
  end function
 
  pure recursive function evaluate(params, binaryOps, unaryOps) result(res)
    character(len=32), allocatable, dimension(:), intent(in) :: params
    character(len=32) :: res
    character(len=32), dimension(6), intent(in) :: binaryOps
    character(len=32), dimension(1), intent(in) :: unaryOps
    integer :: i

    if (size(params) == 1) then
      res = params(1)
    else
      do i=1,size(params)

        if (any(binaryOps == params(i))) then
          res = evaluate(doBinaryOp(params, i), binaryOps, unaryOps)
          exit
        else if (any(unaryOps == params(i))) then
          res = evaluate(doUnaryOp(params, i), binaryOps, unaryOps)
          exit
        endif

      end do
    endif

  end function

end module

program kaboop
use kaboop_funcs
implicit none

  character(len=32), dimension(6) :: binaryOps
  character(len=32), dimension(1) :: unaryOps

  binaryOps(1) = "+"
  binaryOps(2) = "-"
  binaryOps(3) = "/"
  binaryOps(4) = "*"
  binaryOps(5) = "^"
  binaryOps(6) = "**"

  unaryOps(1) = "factorial"

  print *, evaluate(getArgs(), binaryOps, unaryOps)
 
end program
