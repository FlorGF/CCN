program p1e1g
  implicit none
  integer(4) :: n_1, n_2, i
  real(4) :: x
write(*,*) "Ingrese el primer numero natural"
read(*,*) n_1
write(*,*) "Ingrese el segundo numero natural"
read(*,*) n_2
write(*,*) "Ingrese un numero real"
read(*,*) x

if(x<10) then
  do i=1,n_1
    write(*,*) "El numero real es menor que 10"
  end do
else
  do i=1,n_2
    write(*,*) "La raiz del numero real es", sqrt(x)
  end do
end if
end program p1e1g
