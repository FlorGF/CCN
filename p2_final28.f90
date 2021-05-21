program p2_final28 
    implicit none 
    real(8), dimension(2) :: x, y, yp 
    integer(8) :: n, i  
    real(8), allocatable, dimension(:,:) :: res 
    real(8) :: h 
    yp(1)=0 
    yp(2)=10
    h=0.05 
    x(1)=1 
    x(2)=2 
    y(1)=1
    y(2)=3 
    n=(x(2)-x(1))/h 
    allocate(res(n+1,3))
    open(unit=10, file="resltados_p2_28.dat")
    call disparo(x, y, yp, h, res)
    do i=1, n+1
        write(10,*)res(i,1), res(i,2)
    enddo 
    close(10)
    deallocate(res)

end program 
!----------------------------------------------------------------------------------------------------
subroutine disparo(x, y, yp, h, res)
    
    real(8), dimension(2) :: x, y, yp 
    integer(8) :: n !cantidad de pasos de integracion para cada disparo 
    real(8), dimension(n+1,3) :: res 
    integer(8) :: i 
    real(8) :: h, t, a, b, y0 
    real(8),dimension(2) ::  y_aux 
    
    n=(x(2)-x(1))/h 

    !Primer disparo 
    !condiciones iniciales 
    t=x(1)
    y_aux(1)=y(1)
    y_aux(2)=yp(1)
    do i=1, N 
        call heun(t, y_aux, h)
    enddo 
    a=y_aux(1) 
    write(*,*)a, "este es a" 


    !Segundo disparo 
    !condiciones iniciales 
    t=x(1)
    y_aux(1)=y(1)
    y_aux(2)=yp(2)
    do i=1, N 
        call heun(t, y_aux, h)
    enddo 
    b=y_aux(1)
    write(*,*)b, "este es b" 

    
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
        CALL heun(t, y_aux, h)
        res(i+1,1)=t
        res(i+1,2)=y_aux(1)
        res(i+1,3)=y_aux(2)
    enddo 
    
end subroutine 
!----------------------------------------------------------------------------------
subroutine heun(x, y, h)
    use deriv_p7 
    implicit none 
    real(8), intent(inout) :: x
    real(8), intent(inout), dimension(2) :: y 
    real(8), intent(in) :: h !este es el paso 
    real(8), dimension(2) :: k1, k2

    k1=deriv(x,y)
    k2=deriv(x+h,y+h*k1)
    y=y+(k1+k2)*h/2
    x=x+h 

end subroutine 