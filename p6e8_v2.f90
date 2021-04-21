program p6e8 
    implicit none 
    real(8), dimension(2) :: intervalo_x, intervalo_y
    integer :: N, i
    real(8) :: x_integ, y_integ, h 
    
    h=(intervalo_x(2)-intervalo_x(1))/real(N) 


end program 
!------------------------------------------------------
!acá defino las funciones dentro de la integral
function f(x,y)
    implicit none 
    real(8), intent(in) :: x, y
    real(8), intent(out) :: f 
    f=1 !caso masa
    f=x**2+y**2 !caso inercia 
    

end function 
function ext(y)
    implicit none 
    real(8), intent(in) :: y 
    real(8), dimension(2), intent(out) :: ext 
    ext(1)=sqrt(25-y**2)
    ext(2)=sqrt(-25-y**2)
end function 
!---------------------------------------------------------
!subturina simpson 
subroutine simpson_interna(a, b, N, integ)
    integer, intent(in) :: N 
    real(8), intent(in) :: a, b 
    real(8), intent(out) :: integ 
    !variables auxiliares
    real(8) :: h, sum, x
    integer :: i, alfa 
    !este método tiene la restricción de q N es impar, primero hay que chequear esto 
    if(mod(N,2)==0)then 
        write(*,*)"N es par no se puede aplicar el método"
        stop !salgo del programa 
    else 
        write(*,*)"N es impar, estamos bien" 
    endif 
    h=(b-a)/(N-1) 
    sum=0 

    do i=2, N  !esta suma no deme incluir los valores extremos (los agrego a mano despues)
        x=a+(i-1)*h 
        if(mod(i,2)==0)then 
            sum=sum+2.0*f(x)
        else
            sum=sum+4.0*f(x)
        endif 
       ! write(*,*)"sum=", sum 
    enddo 
    sum=sum+f(a)+f(b)
    integ=sum*h/3.0 

end subroutine 
subroutine simpson_externa(a,b,N,integ)
    integer, intent(in) :: N 
    


end subroutine 