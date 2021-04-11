module metodo_integrales
    use my_funcs_integ
    implicit none 
contains
!---------------------------------------------------
    !MÉTODO DEL TRAPECIO
subroutine trapecio(a, b, integ, N)
    !a, y b son los extremos del intervalo, integ el resultado de la integral y N cantidad de subdivisiones 
    !variables principales
    integer, intent(in) :: N 
    real(8), intent(in) :: a, b 
    real(8), intent(out) :: integ 
    
    !variables auxiliares
    real(8) :: h, sum, x_ant, x_new
    integer :: i 

    !implementación del método 
    h=(b-a)/N !medida de los intervalos
    x_ant=a  
    sum=0.0 !seteo inicial 
    do i=1, N 
        x_new=x_ant+i*h
        sum=sum+(f(x_ant)+f(x_new))*h/2.0 
        x_ant=x_new !reseteo de nuevo
    enddo 
    integ=sum 
end subroutine 









end module