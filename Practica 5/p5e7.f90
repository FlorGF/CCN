program p5e7 
    use fitting 
    implicit none 
    integer :: n_param=2
    integer :: n_dat=5
    real(8), allocatable :: x(:), y(:), a(:), a_sem(:)
    real(8) :: tol 
    integer :: i 

    allocate(x(n_dat))
    allocate(y(n_dat))
    allocate(a(n_param))
    allocate(a_sem(n_param))

    write(*,*)"ingrese los datos"
    do i=1, n_dat 
        read(*,*)x(i), y(i) 
    enddo 
    write(*,*)"ingrese la tolerancia"
    read(*,*)tol 
    write(*,*)"ingrese el vector semilla"
    read(*,*)a_sem 
    call gauss_newton(x, y, n_dat, n_param, tol, a, a_sem)
    write(*,*)"los parametros de ajustes son"
    write(*,*)a 


end program 