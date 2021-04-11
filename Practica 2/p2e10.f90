program p2e10
    use my_funcs 
    implicit none 
    real(8) :: x, h, y0, x0, alfa, v0
    real(8), dimension(order) :: y 
    integer :: i, n_step 
!en este caso y tiene cuatro componentes x, y, vx, vy en ese orden 

    !busqueda de datos
    write(*,*)"Ingrese la velocidad inicial"
    read(*,*)v0 
    write(*,*)"Ingrese el angulo de ingreso"
    read(*,*)alfa 
    write(*,*)"Ingrese la posicion inicial en y"
    read(*,*)y0
    write(*,*)"Ingrese la posicion inicial en x"
    read(*,*)x0
    write(*,*)"Ingrese h" 
    read(*,*)h 

!seteo los datos iniciales 
    x=0 !esto es tiempo, arrancamos desde cero 
    y(1)=x0 
    y(2)=y0 
    y(3)=v0*sin(alfa)
    y(4)=v0*cos(alfa)

open(unit=10,file="resultadose10.dat")
    do while (y(2)>0) 
        call rk4(x, y, h)
        write(10,*)x, y 
    enddo 
    close(10)
end program 