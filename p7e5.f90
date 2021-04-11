program p7e5
    use edo_fronteras
    implicit none 
    real(8), dimension(2) :: x, y 
    integer :: n, i
    real(8), allocatable, dimension(:,:):: res 
    x(1)=0.0
    x(2)=10.0 
    y(1)=240.0
    y(2)=150.0 
    write(*,*)"ingrese n"
    read(*,*)n 
    allocate(res(n+1,2))
    call dif_finitas_cte(x, y, n, res)
    open(unit=10,file="resulp7e5_2.txt")
    do i=1,n+1
    write(10,*)res(i,:)
    enddo
    close(10)
    deallocate(res)


    
end program