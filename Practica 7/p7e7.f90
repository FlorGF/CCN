program p7e7 
use edo_fronteras
implicit none 

real(8), dimension(2) :: x, y, yp 
integer(8) :: N , i
real(8), allocatable, dimension(:,:):: res 

open(unit=10, file="datosp7e7.dat",status="old")
read(10,*)x
read(10,*)y
read(10,*)yp 
read(10,*)N 
close(10)
allocate(res(n+1,3))
call disparo(x, y, yp, N, res)

do i=1,N+1 
   if(abs(res(i,1)-0.5)<0.001) write(*,*)"El valor en 0.5 es", res(i,2)
   if(abs(res(i,1)-1.5)<0.001) write(*,*)"El valor en 1.5 es", res(i,2)
enddo 




end program