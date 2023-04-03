program midpoint
    ! Nicholas Maynard
    ! CSI 501
    ! Homework 9
    ! 04/06/2023
    ! The program uses the midpoint rule to compute an integral.
  
    ! Clear out the memory
    implicit none
  
    ! Declare our Variables
    real :: lowerx, upperx, x, h, mpoint, Func, IntFunc, actualVal
    real :: ival = 0.0
    integer :: i
  
    ! Ask the user for our input variables
    print*,'Enter a number for lower x bound:'
    read*, lowerx
    print*, 'Enter a number for upper x bound:'
    read*, upperx
    print*,'Enter a number h:'
    read*, h
  
    ! Calculate our integral for our bounds
    do i = int(lowerx), int((upperx - lowerx)/h)
      
      ! Generate the x value we will use.
      x = lowerx + (float(i - 1) * h) + (0.5 * h)

      ! Calculate the midpoint
      mpoint = Func(x)

      ! Add to our integral estimation
      ival = ival + (mpoint * h)
    enddo

    ! Calculate the real value
    actualVal = IntFunc(3.0) - IntFunc(0.0)

    ! Output the results.
    print*, 'estimated integral with midpoint is: ', ival
    print*, 'actual integral based on math is: ', actualVal
    print*, 'absolute error is: ', abs(ival - actualVal)
    print*, 'relative error is: ', abs((ival - actualVal) / actualVal) * 100.0
  
  
  
  end program midpoint
  
  function Func(x) result(y)
    ! Remove space in memory
    implicit none
    ! Initialize variables for the function
    real :: x, y
  
    ! Create our function
    y = exp(-x) * sin(x)**2
  
  end function

  function IntFunc(x) result(y)
    ! Clear memory
    implicit none
    ! Initialize variables for the function
    real :: x,y

    ! Create our integral function
    y = (exp(-x) / 10) * (cos(2*x) - 2*sin(2*x) - 5)
  end function