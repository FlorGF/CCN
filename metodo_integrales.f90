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
    h=(b-a)/(N-1) !medida de los intervalos
   
    x_ant=a  
    sum=0.0 !seteo inicial 
    integ=0 
    
    do i=1, N 
        x_new=a+(i-1)*h
        
        sum=sum+(f(x_ant)+f(x_new))
        
        x_ant=x_new !reseteo de nuevo
    enddo 
    integ=sum*h/2.0 
end subroutine 

!---------------------------------------------------
subroutine simpson(a, b, N, integ)
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

    do i=2, N-1  !esta suma no deme incluir los valores extremos (los agrego a mano despues)
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

!---------------------------------------------------------------------------------------
subroutine splines_integ(a, b, N, integ) 
    real(8) :: a, b, integ, h
    integer :: N , i
    real(8), dimension(n) :: x, y, y2 !y2 es la derivada segunda de la función 
    real(8) :: yp1, ypn !estos son datos para la subrutina de splines
    h=(b-a)/(n-1) 

    !armo los arreglos x e y 
    do i=1, n 
        x(i)=a+(i-1)*h
        y(i)=f(x(i))
    enddo 
    
    yp1=f(x(1))
    ypn=f(x(n))

    !ahora llamo a splines para calcular y2
    call spline(x, y, n, yp1, ypn, y2)

    integ=0.0 
    !esta es la fórmula final para encontrar la integral 
    DO i=1, n-1
        integ=integ+ &
        (x(i+1)-x(i))/2._8* &
        (y(i)+y(i+1)- &
        (x(i+1)-x(i))**3/12._8*(y2(i)+y2(i+1)))
    ENDDO


end subroutine 
!------------------------------------------------------------
SUBROUTINE spline(x, y, n, yp1, ypn, y2)
    
       
        INTEGER :: n, i, k
        REAL(8), DIMENSION(n), INTENT(IN) :: x, y
        REAL(8), DIMENSION(n), INTENT(OUT) :: y2
        REAL(8), DIMENSION(n) :: u
        REAL(8), INTENT(IN) :: yp1, ypn
        REAL(8) :: p,qn,sig,un
        IF (yp1.GT..99e30) THEN
            y2(1) = 0.
            u(1) = 0.
        ELSE
            y2(1) = -0.5
            u(1) = (3./(x(2)-x(1)))*((y(2)-y(1))/(x(2)-x(1))-yp1)
        ENDIF
        DO i = 2,n-1
            sig = (x(i)-x(i-1))/(x(i+1)-x(i-1))
            p = sig*y2(i-1)+2.
            y2(i) = (sig-1.)/p
            u(i) = (6.*((y(i+1)-y(i))/(x(i+1)-x(i))-(y(i)-y(i-1)) &
            & /(x(i)-x(i-1)))/(x(i+1)-x(i-1))-sig*u(i-1))/p
        ENDDO
        IF (ypn.gt..99e30) THEN
            qn = 0.
            un = 0.
        ELSE
            qn = 0.5
            un = (3./(x(n)-x(n-1)))*(ypn-(y(n)-y(n-1))/(x(n)-x(n-1)))
        ENDIF
        y2(n) = (un-qn*u(n-1))/(qn*y2(n-1)+1.)
        DO k = n-1,1,-1
            y2(k) = y2(k)*y2(k+1)+u(k)
        ENDDO
        RETURN
    END SUBROUTINE spline

!---------------------------------------------------------------------------------------
  







end module