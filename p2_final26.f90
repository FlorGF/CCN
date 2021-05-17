program p2_final26
    implicit none 
    real(8), allocatable, dimension(:) :: x 
    integer :: N, i
    real(8) :: integ 
    open(unit=10, file="datos_p2_26.dat",status="old")
    read(10,*)N 
    allocate(x(n))
    do i=1, n 
        read(10,*)x(i)
    enddo 
    call splines_integ(x,N,integ)
    write(*,*)"El valor de la integral es", integ 


end program 
!------------------------------------------------------------------------------------------------------
subroutine splines_integ(x, N, integ) 
    real(8) ::  integ, h
    integer :: N , i
    real(8), dimension(n) :: x, y, y2 !y2 es la derivada segunda de la función 
    real(8) :: yp1, ypn, f !estos son datos para la subrutina de splines (el valor de la derivada primera)
    

    !armo los arreglos x e y 
    do i=1, n 
        
        y(i)=f(x(i))
    enddo 
    
    yp1=f(x(1))
    ypn=f(x(n)) !estos pueden ser datos, por qué las calcule así? 

    !ahora llamo a splines para calcular y2 (los valores de la derivada segunda en f(xi))
    call spline(x, y, n, yp1, ypn, y2)

    integ=0.0 
    !esta es la fórmula final para encontrar la integral (está escrita en la práctica)
    DO i=1, n-1
        integ=integ+ &
        (x(i+1)-x(i))/2._8* &
        (y(i)+y(i+1)- &
        (x(i+1)-x(i))**3/12._8*(y2(i)+y2(i+1)))
    ENDDO


end subroutine 
!--------------------------------------------------------------------------------------------------------
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
!--------------------------------------------------------------------------------------------------------------------
function f(x)
    implicit none 
    real(8):: f
    real(8), intent(in) :: x 
    f=exp(-x*x)
end function 