program prueba_sistemas
    use sistemas_de_ecuaciones
    implicit none 
    !real(8) :: tol
    !real(8), dimension(n):: B, X, x_sem
    real(8), dimension(n,n) :: A, Ainv
    integer :: i 
    open(unit=10, file="ejemploinversa.dat", status="old")
    do i=1, n 
        read(10,*)A(i,:) 
    enddo 
   ! do i=1, n 
    !    read(10,*)B(i) 
   ! enddo 
    close(10)
   ! write(*,*)"Ingrese la tolerancia"
    !read(*,*)tol
   ! write(*,*)"ingrese la semilla"
   ! do i=1,n
   ! read(*,*)x_sem(i)
    !enddo  
    call inversa(A, Ainv)
    write(*,*)"la inversa es"
    do i=1, n 
        write(*,*)Ainv(i,:)
    enddo 
end program 
     