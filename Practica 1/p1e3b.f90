program p1e3b
  real(8), dimension(15) :: a
  real(8) :: h, pi, n
  n=15
  pi=4.0*atan(1.0)
  h=(2*pi-pi)/n
  do i=0, 15
    x=i*h+pi
    a(i)=x
  end do
  write(*,*) a
end program p1e3b
