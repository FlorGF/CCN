program parcial_ej2 
    use funciones 
    implicit none 
    real(8) :: tol, x_sem, iter_max, x, err, iter 
    open(unit=10,file="datos_parcialej2.dat",status="old")
    read(10,*)tol
    read(10,*)x_sem
    read(10,*)iter_max 

    close(10)

    call newton(x_sem, tol, x, err, iter, iter_max)
    
    write(*,*)"la raiz y la funcion en la raiz es" 
    write(*,*)x, f(x)
    write(*,*)"el error es" 
    write(*,*)err
    write(*,*)"la cantidad de iteraciones q se hicieron"
    write(*,*)iter 




end program 

!--------------------------------------
subroutine newton(x_sem, tol, x, err, iter, iter_max)
    use funciones 
    implicit none 
    real(8), intent(in) :: iter_max 
    real(8), intent(inout) :: x_sem, tol 
    real(8), intent(out) :: x, err, iter 
    real(8) :: x_1
    
    !armo el primer x_n 
    x_1=x_sem-f(x_sem)/df(x_sem) 
    iter=0
    !armo la iteración 
     
    do 
        if((abs(x_1-x_sem)<tol .and. abs(f(x_1))<tol) .or. iter>iter_max)then 
            x=x_1 
            err=abs(x_1-x_sem)
            exit 
        else 
            x_sem=x_1
            
        endif
        x_1=x_sem-f(x_sem)/df(x_sem)
        iter=iter+1.0
    enddo 
end subroutine 

!--------------------------------------
module funciones
    implicit none 
contains 
function f(x)
    real(8), intent(in)  :: x
    real(8) :: f 
    f=2.0*x**(-12)-3.0*x**(-6)+0.1

end function 

function df(x)
    real(8), intent(in)  :: x
    real(8) :: df 
    df=-24.0*x**(-11)+18.0*x**(-5)

end function 

end module 