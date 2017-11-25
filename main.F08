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

  pure function doAdd(params, i) result(newParams)
    character(len=32), allocatable, dimension(:), intent(in) :: params
    character(len=32), allocatable, dimension(:) :: newParams
    integer, intent(in) :: i
    double precision :: res, a, b

    allocate(newParams(size(params) - 2))

    read(params(i-2),*) a
    read(params(i-1),*) b

    res = a + b

    if (i > 3) then
      newParams(1:(i - 3)) = params(1:(i - 3))
    endif

    write(newParams(i-2), *) res
    newParams(i-1:(size(newParams))) = params((i+1):(size(params)))
  end function

  pure recursive function evaluate(params) result(res)
    character(len=32), allocatable, dimension(:), intent(in) :: params
    character(len=32) :: res
    integer :: i

    if (size(params) == 1) then
      res = params(1)
    else
      do i=1,size(params)
        if (params(i) == "+") then
          res = evaluate(doAdd(params, i))
          exit
        endif
      end do
    endif

  end function

end module

program kaboop

  use kaboop_funcs
  print *, evaluate(getArgs())
 
end program
