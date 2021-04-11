program p5e11
    use splines 
    implicit none 
    real(8), allocatable :: x(:), y(:), y2(:)
    real(8) :: yp1,ypn !los valores de la derivada que los seteamos en cero 
    real(8) :: x_int,y_int
    integer :: n_int, n_dat, i
    open(unit=10,file="p5_ej11.dat")
    read(10,*)n_dat 
    allocate(x(n_dat),y(n_dat),y2(n_dat))
    do i=1, n_dat 
        read(10,*)x(i), y(i)
    enddo 
    close(10)
    yp1=0.0 
    ypn=0.0


    call spline (x,y,n_dat,yp1,ypn,y2)

    write(*,*)"ingrese la cantidad de números a interpolar"
    read(*,*)n_int 
    open(unit=11,file="resultadosp5e11")
    do i=1, n_int
        x_int=x(1)+(x(n_dat)-x(1))*real(i-1)/real(n_int-1)
        call tercer_orden(x, y, y2, n_dat, x_int, y_int)
        write(11,*)x_int, y_int
    enddo 
    close(11)
    deallocate(x, y, y2) 


end program 