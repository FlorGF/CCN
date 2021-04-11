program p4e3
    use sistemas_de_ecuaciones
    implicit none
    real(8), dimension (n,n) :: A 
    real(8), dimension(n) :: B, X, x_sem 
    real(8) :: tol
    integer :: i
    open(unit=10, file="datosp4e3.dat", status="old")
    do i=1, n 
        read(10,*)A(i,:), B(i)
    enddo 

    
    close(10)
    call gauus_total(A, B, X)
    write(*,*)"solucion con gauss" 
    write(*,*)X 

    call gauss_jordan_total(A, B, X)
    write(*,*)"solucion con gauss jordan"
    write(*,*)X 

    write(*,*)"ingrse la tolerancia" 
    read(*,*)tol 
    write(*,*)"ingrese la semilla"
    read(*,*)x_sem 

    call jacobi(A,B,x_sem,x, tol)
    write(*,*)"solucion con jacobi" 
    write(*,*)x 
    write(*,*)matmul(A,x)-b
end program 

