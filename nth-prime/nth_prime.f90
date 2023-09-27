module nth_prime
  implicit none
contains

  ! get nth prime
  integer function prime(n)
    integer, intent(in) :: n
    integer :: found

    prime = 2
    found = 1
    
    if (n < 1) then
      prime = -1
    else
      do while (found < n)
        prime = prime + 1
        do while (.not. is_prime(prime))
          prime = prime + 1
        end do
        found = found + 1
      end do
    end if
  end function

  logical function is_prime(number)
    integer, intent(in) :: number
    integer :: divisor
    is_prime = .true.
    divisor = 2
    do while (divisor*divisor <= number)
      if (mod(number, divisor) == 0) is_prime = .false.
      divisor = divisor + 1
    end do
  end function
end module
