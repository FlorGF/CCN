program prueba_nolineales
    use sist_no_lineales
    implicit none 
    integer :: n, i 
    real(8), allocatable, dimension(:) :: x, x_sem 
    real(8) :: tol 

    write(*,*)"ingrese la cantidad de ecuaciones=incognias"
    read(*,*)n
    allocate(x(n))
    allocate(x_sem(n))
    write(*,*)"Ingrese la tolerancia"
    read(*,*)tol 
    write(*,*)"ingrese la semilla"
    do i=1, n
    read(*,*)x_sem(i)
    enddo
    call newton_raphson(x, x_sem, tol)

    write(*,*)"La solucion es"
    write(*,*)x 
    deallocate(x, x_sem)

end program 