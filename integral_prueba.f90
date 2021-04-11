program integral_prueba 
use metodo_integrales 
implicit none 


integer :: N 
real(8) :: a, b, integ 

write(*,*)"Ingrese el intervalo"
read(*,*)a, b 
write(*,*)"Ingrese la cantidad de subintervalos"
read(*,*)N 
!call trapecio(a, b, integ, N) 
!call simpson(a, b, N, integ)
call splines_integ(a, b, N, integ) 
write(*,*)"El valor de la integral en el intervalo indicado es", integ 







end program