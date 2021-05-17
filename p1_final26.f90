program p1_final26 
    implicit none 
    real(8), dimension(2) :: x, y, yp 
    integer(8) :: n , i
    real(8), allocatable, dimension(:,:) :: res 
    real(8) :: h 
    x(1)=0 
    x(2)=10 
    y(1)=0 
    y(2)=120 
    h=0.2 
    N=(x(2)-x(1))/h 
    yp(1)=0 
    yp(2)=10 
    allocate(res(n+1,3))

    call disparo(x, y, yp, N, res)

    open(unit=10,file="posicion-p1_26.dat")
    open(unit=11,file="velovidad-p1_26.dat")
    do i=1,n 
        write(10,*)res(i,1), res(i,2)
        write(11,*)res(i,1), res(i,3)
    enddo 

    close(10)
    close(11)
    deallocate(res)




end program 
!------------------------------------------------------------------------------------------------------------
subroutine disparo(x, y, yp, N, res)
    real(8), dimension(2) :: x, y, yp 
    integer(8) :: n 
    real(8), dimension(n+1,3) :: res 
    integer(8) :: i 
    real(8) :: h, t, a, b, y0 
    real(8),dimension(2) ::  y_aux 
   
    h=0.2 
   !primer disparo
    t=x(1)
    y_aux(1)=y(1)
    y_aux(2)=yp(1)
    do i=1, N 
        call rk4(t, y_aux, h)
    enddo 
    a=y_aux(1) 
    
    !Segundo disparo
    t=x(1)
    y_aux(1)=y(1)
    y_aux(2)=yp(2)
    do i=1, N 
        call rk4(t, y_aux, h)
    enddo 
    b=y_aux(1)

    !derivada en el segundo extremo real 
    y0=((Yp(2)-Yp(1))/(b-a))*(Y(2)-a)+Yp(1)

 !Tercer y último disparo 
 !condiciones iniciales 
    t=x(1)
    y_aux(1)=y(1)
    y_aux(2)=y0 

    !armo la primer fila de res 
    res(1,1)=t              !ext izq
    res(1,2)=y_aux(1)       !func ext izq
    res(1,3)=y_aux(2)       !deriva ext izq
    
    !calculo los valores que faltan 

    DO i=1, n
        CALL rk4(t, y_aux, h)
        res(i+1,1)=t
        res(i+1,2)=y_aux(1)
        res(i+1,3)=y_aux(2)
    enddo 
end subroutine 
!-------------------------------------------------------------------------------------------
subroutine rk4(x, y, h)
    use deriv_p7 
    implicit none 
    real(8), intent(inout) :: x
    !real(8), dimension(2) :: deriv 
    real(8), intent(inout), dimension(2) :: y 
    real(8), intent(in) :: h !este es el paso 
    real(8), dimension(2) :: k1, k2, k3, k4 

    k1=deriv(x,y)
    k2=deriv(x+h/2.0,y+(h/2.0)*k1)
    k3=deriv(x+h/2.0,y+(h/2.0)*k2)
    k4=deriv(x+h,y+h*k3)
    y=y+(k1+2.0*(k2+k3)+k4)*h/6.0
    x=x+h
end subroutine
