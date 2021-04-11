program p2e5
    use my_funcs 
    implicit none 
    real(8) :: x, h, theta0, omega0, x_i
    real(8), dimension(order) :: y 
    integer :: i, n_step
    write(*,*)"Ingrese la condición inicial en theta"
    read(*,*)theta0 
    write(*,*)"Ingrese la condicion inicial en omega"
    read(*,*)omega0 
    write(*,*)"Ingrese el paso, h"
    read(*,*)h 
    write(*,*)"Ingrese el inicio de x"
    read(*,*)x_i 
    write(*,*)"Ingrese los pasos de integración"
    read(*,*)n_step 
    y(1)=theta0 
    y(2)=omega0 
    x=x_i 
    open(unit=10,file="Resultadosp2e5_1",status="new")
    do i=1, n_step 
        call rk4(x, y, h)
        write(10,*)x, y(1) 
    enddo 
    close(10)
end program 