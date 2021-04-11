program ej2parcial2019
    integer(8) :: m, n 
    real(8), allocatable, dimension(:,:) :: a 
    real(8), allocatable, dimension(:) :: s
    write(*,*)"Ingrese los ordenes de ma matriz MxN"
    read(*,*)m, n 
    allocate(a(m,n),s(n))
    call matriz(m, n, a)
    call suma(a, m, n, s)
    write(*,*)"La madtriz:"
    do i=1, m
    write(*,*)a(i,:)
    
    enddo
    write(*,*)"La suma es"
    do i=1, n
        write(*,*)s(i)
    enddo
    deallocate(a)
    deallocate(s)
end program 