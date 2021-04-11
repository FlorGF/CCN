program ejemplo_ajusteslineal 
    use fitting 
!este programa tambien lo uso en los ejercicios 2 y 3
    implicit none 
    integer :: n_param=3
    integer :: n_dat=4 
    real(8), allocatable :: x(:), y(:), a(:)
    real(8) :: err
    integer :: i 
    allocate(x(n_dat))
    allocate(y(n_dat))
    allocate(a(n_param))

    write(*,*)"ingrese los datos"
    do i=1, n_dat 
        read(*,*)x(i), y(i) 
    enddo 
    call ajuste_lienal(x, y, n_dat,n_param, a, err)
    write(*,*)"La función que ajusta es" 
    write(*,*)a(1), a(2), "x", a(3), "x**2"
    write(*,*)"El error es" 
    write(*,*)err 

    deallocate(x, y, a)



end program 