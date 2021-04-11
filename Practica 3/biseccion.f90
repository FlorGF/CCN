subroutine biseccion(a, b, tol, x, err, iter)
    use funciones 
    implicit none 
    real(8), intent(inout) :: a, b, tol 
    real(8), intent(out) :: x, err 
    real(8) :: c, iter

    !me fijo si se le puede aplicar este método 
    if(sign(1.0_8,f(a))==sign(1.0_8,f(b)))then 
        write(*,*)"No se le puede aplicar el método"
        stop  
    endif 

    !construyo la iteración 
    c=(b+a)/2 !seteo el primer valor de c
    iter=0 
   
    do 
    
         if(abs(b-a)/2<tol .and. abs(f(c))<tol)then
             x=c 
            err=abs(b-a)/2
            exit 
        elseif(sign(1.0_8,f(c))==sign(1.0_8,f(a)))then 
            
            a=c
        else      !cambio los intervalos
            
            b=c
        endif 
        c=(b+a)/2 !cambio c 
        iter=iter+1.0
    enddo 

end subroutine 