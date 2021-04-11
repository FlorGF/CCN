program p5e10
    use interpolacion 
    implicit none 
    integer :: n_dat
    real(8), allocatable :: x_dat(:), y_dat(:)
    real(8) :: x_int, h, y_int  
    integer :: n_iter, i 
    n_iter=101
    open(unit=10, FILE="p5_ej10.dat", status="old") 
    read(10,*)n_dat 
    allocate(x_dat(n_dat))
    allocate(y_dat(n_dat))
    do i=1, n_dat 
        read(10,*)x_dat(i), y_dat(i) 
    enddo 
    close(10)
    
    h=(maxval(x_dat)-minval(x_dat))/n_iter
    x_int=minval(x_dat)
    open(unit=11, file="resul_p5e10")
    do i=1, n_iter
        x_int=x_int+(i-1)*h 
        y_int=lagrange( x_dat, y_dat, x_int)
        write(*,*)x_int, y_int 
        write(11,*)x_int, y_int 
    enddo 
    close(11)
    deallocate(x_dat, y_dat)




end program 