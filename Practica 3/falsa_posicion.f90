subroutine falsa_posicion(a, b, tol, x, err, iter)
    use funciones 
    implicit none 
    real(8), intent(inout) :: a, b, tol 
    real(8), intent(out) :: x, err, iter 
    real(8) :: c, aux

     !me fijo si se le puede aplicar este método 
    if(sign(1.0_8,f(a))==sign(1.0_8,f(b)))then 
        write(*,*)"No se le puede aplicar el método"
        stop  
    endif 

    c=b-(f(b)*(b-a))/(f(b)-f(a))
    aux=(b+a)/2 
    iter=0
!hago la iteración 
    do 
        !write(*,*)c, f(c)
        if(abs(c-aux)<tol .and. abs(f(c))<tol)then
            x=c 
           err=abs(c-aux)
           exit 
       elseif(sign(1.0_8,f(c))==sign(1.0_8,f(a)))then 
           
           a=c
       else      !cambio los intervalos
           
           b=c
       endif 
       aux=c
       c=b-(f(b)*(b-a))/(f(b)-f(a))!cambio c 
       iter=iter+1.0
   enddo 

end subroutine 
