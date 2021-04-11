program p2e6
    use my_funcs 
    implicit none 
    real(8) :: x, h, t0, y1, y2, y3, y4 !y1, y2, y3, y4, t0 son las condiciones iniciales
    real(8), dimension(order) :: y 
    integer :: i, n_step 
    n_step=100
    h=0.1 
    open(unit=10,file="datose6.dat",status="old")
    read(10,*)t0 
    read(10,*)y1 
    read(10,*)y2 
    read(10,*)y3 
    read(10,*)y4 
    close(10)

    !setel el vector y 
    y(1)=y1
    y(2)=y2
    y(3)=y3
    y(4)=y4 
    x=t0

    open(unit=11,file="resultadose6_1",status="new")
     
    do i=1, n_step 
        call rk4(x, y, h)
        write(11,*)x, y
    enddo 
    close(11)
    
end program 