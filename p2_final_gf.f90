program p2_final
    implicit none 
    real(8), dimension(2) :: x_ext, y_ext, yp 
    real(8) :: h 
    real(8), allocatable, dimension(:,:) :: res 
    integer :: i, n 
    h=0.05 
    x_ext(1)=1.0
    x_ext(2)=2.0 
    y_ext(1)=1.0
    y_ext(2)=3.0 
    yp(1)=0.0 
    yp(2)=10.0 

    n=(x_ext(2)-x_ext(1))/h 
    allocate(res(n+1,2))
    call disparo(x_ext, y_ext, yp, n, h, res)
    open(unit=11,file="resultados-GomezFava_2.dat")

    do i=1, n+1
        write(11,*)res(i,1), res(i,2)
    enddo 

    close(11)
    deallocate(res)
end program 
!-------------------------------------------------------
subroutine heun(x, y, h)
    use derivs 
    implicit none 
    real(8), intent(inout) :: x
    real(8), intent(in) :: h 
    real(8), intent(inout), dimension(2) :: y 
    real(8), dimension(2) :: k1, k2


    k1=deriv(x,y)
    k2=deriv(x+h,y+h*k1)
    y=y+(k1+k2)*h/2
    x=x+h 

end subroutine
!-------------------------------------------------------

subroutine disparo(x_ext, y_ext, yp, n, h, res)
    implicit none 
    real(8), intent(in), dimension(2) :: x_ext, y_ext, yp 
    !x_ext contiene los extremos en x, y_ext contiene los extremos en y, e yp tiene los valores de la derivada primera
    !en el primer extremo para hacer los disparos
    real(8), intent(in) :: h 
    integer, intent(in) :: n 
    integer :: i 
    real(8),intent(out), dimension(n+1,2) :: res 
    real(8) :: x, a , b, y0
    real(8), dimension(2) :: y_aux
    

    !Primer disparo 
    x=x_ext(1)
    y_aux(1)=y_ext(1)
    y_aux(2)=yp(1)
    do i=1, n 
        call heun(x, y_aux, h)
    enddo 
    a=y_aux(1)

    !Segundo disparo
    x=x_ext(1)
    y_aux(1)=y_ext(1)
    y_aux(2)=yp(2)
    do i=1, n 
        call heun(x, y_aux, h)
    enddo 
    b=y_aux(1)

    y0=((yp(2)-yp(1))/(b-a))*(y_ext(2)-a)+yp(1)

    res(1,1)=x_ext(1)
    res(1,2)=y_ext(1)
   
    
    x=x_ext(1)
    y_aux(1)=y_ext(1)
    y_aux(2)=y0
    do i=1,n 
        call heun(x, y_aux, h)
        res(i+1,1)=x 
        res(i+1,2)=y_aux(1)
    enddo 

   

end subroutine 