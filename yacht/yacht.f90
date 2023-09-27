
module yacht
  implicit none

  ! | Ones | 1 × number of ones | Any combination	| 1 1 1 4 5 scores 3 |
  ! | Twos | 2 × number of twos | Any combination | 2 2 3 4 5 scores 4 |
  ! | Threes | 3 × number of threes | Any combination | 3 3 3 3 3 scores 15 |
  ! | Fours | 4 × number of fours | Any combination | 1 2 3 3 5 scores 0 |
  ! | Fives | 5 × number of fives| Any combination | 5 1 5 2 5 scores 15 |
  ! | Sixes | 6 × number of sixes | Any combination | 2 3 4 5 6 scores 6 |
  ! | Full House | Total of the dice | Three of one number and two of another | 3 3 3 5 5 scores 19 |
  ! | Four of a Kind | Total of the four dice | At least four dice showing the same face | 4 4 4 4 6 scores 16 |
  ! | Little Straight |  30 points | 1-2-3-4-5 | 1 2 3 4 5 scores 30 |
  ! | Big Straight | 30 points | 2-3-4-5-6 | 2 3 4 5 6 scores 30 |
  ! | Choice | Sum of the dice | Any combination | 2 3 3 4 6 scores 18 |
  ! | Yacht | 50 points | All five dice showing the same face | 4 4 4 4 4 scores 50 |


contains

  integer function score(dice, yacht_type)
    integer, dimension(5), intent(in) :: dice
    character(len=*), intent(in) :: yacht_type
    logical, dimension(5) :: mask
    integer :: i, j

    score=0

    select case (yacht_type)
    case ('ones')
      score = 1 * count(dice == 1)
    case ('twos')
      score = 2 * count(dice == 2)
    case ('threes')
      score = 3 * count(dice == 3)
    case ('fours')
      score = 4 * count(dice == 4)
    case ('fives')
      score = 5 * count(dice == 5)
    case ('sixes')
      score = 6 * count(dice == 6)
    case ('four of a kind')
      do i = 1, 5
        mask(i) = (count(dice == dice(i)) >= 4)
      end do
      j = findloc(mask, DIM=1, VALUE=.true.)
      if (j > 0) score = sum(dice, DIM=1, MASK=(dice == dice(j)))
    case ('little straight')
      score = 30
      do i = 1, 5
        if (count(dice(:) == i) /= 1) score = 0
      end do
    case ('big straight')
      score = 30
      do i = 2, 6
        if (count(dice(:) == i) /= 1) score = 0
      end do
    case ('choice')
      score = sum(dice)
    case ('yacht')
      if (count(dice(:) == dice(1)) == 5) score = 50
    end select
  end function


end module
