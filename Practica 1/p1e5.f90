REAL FUNCTION f(x)
  IMPLICIT NONE
  REAL(4) :: x
  f = sin(2 * x) * exp(-x)
END FUNCTION

program p1e4
implicit none
  real(4) :: h, pi, n, x, f
  integer(4) :: i
  n=15
  pi=3.14159
  h=(2*pi-pi)/n
  do i=0, 15
    x=i*h+pi
    write(*,*) f(x)
  end do
end program p1e4
