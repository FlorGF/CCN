program p2e2
    
    implicit none 
    integer(8) :: n_step, i
    real(8) :: x_inf, x_sup, h, y0, x, y 
    !real(8), dimension(order) :: y


     open(unit=10,file="datose2.dat",status="old")
     read(10,*)x_inf 
     read(10,*)x_sup
     read(10,*)h
     read(10,*)y0 
     close(10)

     n_step=(x_sup-x_inf)/h
     write(*,*)n_step 
!seteo las condiciones iniciales 
     y=y0 
     x=x_inf 
     
    do i=1, n_step 
        call euler(x, y, h) 
        write(*,*)x, y 
    enddo 
end program 