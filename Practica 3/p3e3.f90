program p3e3
    implicit none 
    real(8) :: x, err, tol, iter 
    real(8) :: a, b 
    real(8) :: x_0, x_1, aux

    open(unit=10,file="datosp3e3.dat")
    read(10,*)a 
    read(10,*)b 
    read(10,*)tol 
    read(10,*)x_0
    read(10,*)x_1 

    close(10)

    write(*,*)"Elija el metodo, 1:biseccion, 2:falsa posicion"
    write(*,*)"3:newton, 4:secante"
    read(*,*)aux 
     
    if(aux==1)then 
        call biseccion(a, b, tol, x, err, iter)
        write(*,*)"Se eligio el método bisección"
    elseif(aux==2)then 
        call falsa_posicion(a, b, tol, x, err, iter)
        write(*,*)"Se eligio el método falsa posicion"
    elseif(aux==3)then 
        call newton(x_0, tol, x, err, iter)
        write(*,*)"Se eligio el método newton"
    elseif(aux==4)then 
        call secante(x_0, x_1, tol, x, err, iter)
        write(*,*)"Se eligio el método secante"
    endif 


    write(*,*)"la raíz es"
    write(*,*)x
    write(*,*)"El error es:"
    write(*,*)err 
    write(*,*)"las iteraciones q se hicieron"
    write(*,*)iter 
end program 

    