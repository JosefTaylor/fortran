
module acronym
  implicit none
contains

  function abbreviate(s)
    character(len=*), intent(in) :: s
    integer :: i, j, s_length
    character(len=len(s)) :: abbreviate
    logical :: capture
    character :: letter
    character(52) :: alphabet = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ'

    i = 1
    abbreviate = ''
    
    do while (i < len(s))
      ! get the position of the next letter
      i = i + scan(s(i:), alphabet) - 1 
      
      ! concatenate to the output  
      abbreviate = trim(abbreviate) // to_upper(s(i:i))

      ! find the next word-separator
      j = verify(trim(s(i+1:)), alphabet // "'")
      if (j == 0) exit
      i = i + j + 1
    end do

  end function

  character function to_upper(letter)
    character, intent(in) :: letter

    select case (letter)
    case ('a':'z')
      to_upper = char(ichar(letter) + ichar('A') - ichar('a'))
    case default
      to_upper = letter
    end select
  end function

end module
