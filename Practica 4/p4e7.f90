program p4e7
    use sistemas_de_ecuaciones 
    implicit none 
    real(8), dimension(n,n) :: A, Ainv, Id 
    integer :: i 

    open(unit=11,file="datosp4e7.dat",status="old")
    do i=1, n 
        read(11,*)A(i,:)
    enddo 

    close(11)

    write(*,*)A 

    call inversa(A,Ainv)

    write(*,*)"la inversa es" 
    write(*,*)Ainv 

    Id=matmul(A,Ainv) 

    write(*,*)"chequeo" 
    write(*,*)Id

    




end program 