program p1e19
    implicit none 
    integer(8) :: N, i, s  
    real(8) :: x, logaritmo_1, d
write(*,*)"Ingrese el extremo superior de la sumatoria"
read(*,*)N 
write(*,*)"Ingrese el numero a evaluar"
read(*,*)x
if(-1.0<x.AND.x<1.0)then
    write(*,*)"El valor del logaritmo es"
    write(*,*)logaritmo_1(N,x)
else 
    write(*,*)"La serie no converge en ese valor"   
endif
d=abs(logaritmo_1(N,x)-log(x+1))
write(*,*)d
end program 