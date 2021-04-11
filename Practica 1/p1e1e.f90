program p1e1e
implicit none
integer(4):: N
write(*,*) "ingrese un número"
read(*,*) N
if(N<10) then
  write(*,*) "El numero es menor que 10"
else
  write(*,*) "El numero es mayor o igual a 10"
end if
end program p1e1e
