program p1e5
implicit none
  real(8):: x, y
  integer(4) :: i
  do i=0, 20
    x=i/5
    y=i/5.0
    write(*,*) x, y
  end do
end program p1e5
