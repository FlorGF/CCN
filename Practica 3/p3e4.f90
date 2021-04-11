program p3e4
    use funciones
    implicit none 
    real(8) :: a, b, h, x, err, iter, tol
    integer :: n , i, n_step,aux
    real(8), allocatable, dimension(:):: sol
    

    write(*,*)"Indique la cantidad de raices"
    read(*,*)n 
    write(*,*)"Indique el intervalo donde se encuentran las raices"
    read(*,*)a, b
    write(*,*)"Indique la tolerancia"
    read(*,*)tol 
    allocate(sol(n))
    
    n_step=1000
    h=(b-a)/n_step
    aux=1
    !aca recorro todo el intervalo de a pasitos buscando las raices
    do i=1, n_step 
        if(sign(1.0_8,f(a+h*(i-1)))/=sign(1.0_8,f(a+i*h)))then 
            write(*,*)"Hola"
            call biseccion(a+h*(i-1), a+i*h, tol, x, err, iter)
            write(*,*)x, "raiz"
            sol(aux)=x
            aux=aux+1 
        endif 
    enddo 
!aca voy a chequear si las encontro a todas
    if(aux==n+1)then 
        write(*,*)"las raices son" 
        write(*,*)sol 
    else 
        write(*,*)"No encontro todas las soluciones, achicar el h"
    endif 

    deallocate(sol)

end program 



    

    