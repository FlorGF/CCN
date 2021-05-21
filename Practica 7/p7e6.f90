program p7e6 
    use edo_fronteras 
    implicit none 
    integer :: n, i
    !número de puntos interiores 
    real(8), dimension(2) :: x_ext, y_ext 
    real(8), allocatable, dimension(:,:) :: res

    x_ext(1)=0.0
    x_ext(2)=2.0
    y_ext(1)=1.0
    y_ext(2)=-1.0 

    n=19
    allocate(res(n+2,2))

    call dif_finitas_gral(x_ext, y_ext, n, res)

    do i=1,n+2 
        if(abs(res(i,1)-0.5)<0.0002) write(*,*)"El resultado en 0.5 es:", res(i,2) 
        if(abs(res(i,1)-1.0)<0.0002) write(*,*)"El resultado en 1 es:", res(i,2)
        if(abs(res(i,1)-1.5)<0.0002) write(*,*)"El resultado en 1.5 es:", res(i,2)
    enddo 


    deallocate(res)


end program 