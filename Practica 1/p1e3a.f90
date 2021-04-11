program p1e3a
implicit none
real(4) :: h, x
integer(4) :: i
h=0.5
x=0
do i=0, 38
  x=1+i*h
  write(*,*) x
end do
end program p1e3a
