program p2e8 
    use my_funcs 
    implicit none 
    real(8) :: x, h, t0, t1, y0, v0 
    real(8), dimension(order) :: y 
    integer :: i, n_step 
!busco los datos
    write(*,*)"Ingrese los extremos del intervalo de tiempo"
    read(*,*)t0, t1 
    write(*,*)"Ingrese el paso" 
    read(*,*)h 
    write(*,*)"Ingrese la condicion inicial de y"
    read(*,*)y0 
    write(*,*)"Ingrese la condicion inicial de v"
    read(*,*)v0 

!seteo las condiciones iniciales 
    y(1)=y0 
    y(2)=v0 
    x=t0 

!calculo la cantidad de ciclos del do
    n_step=(t1-t0)/h 

!llamo a la subrutina y guardo en un archivo 

    open(unit=10,file="resultadosp2e8_a",status="old")

    do i=1, n_step 
        call rk4(x, y, h)
        write(10,*)x, y 
    enddo 
end program 