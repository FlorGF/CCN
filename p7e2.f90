program p7e2
    use edo_fronteras
    implicit none 
    real(8), dimension(2) :: x, y, yp 
    integer(8) :: N , i
    real(8), allocatable, dimension(:,:):: res 
    open(unit=11,file="datosp7e2.dat",status="old")
    read(11,*)x 
    read(11,*)y 
    read(11,*)yp 
    read(11,*)N 
    close(11) 
    allocate(res(n+1,3))
     
    call disparo(x, y, yp, N, res)
    write(*,*)"final" 
    open(unit=12,file="resulp7e2.dat", status="old")
   do i=1, n+1 
        write(*,*)res(i,:)
        write(12,*)res(i,:)

   enddo 
   close(12)
    deallocate(res)




end program 