integer FUNCTION f(x)
  implicit NONE
  integer(4) :: x
  f=mod(x,2)
end FUNCTION

program p1e6
  implicit NONE
  integer(4) :: x, f
  write(*,*) "Ingrese un número"
  read(*,*) x

  if(f(x)==0) then
    write(*,*) "El numero es par"
  else
    write(*,*)"El numero es impar"
  end if
end program p1e6
