subroutine newton(x_0, tol, x, err, iter)
    use funciones 
    implicit none 
    real(8), intent(inout) :: x_0, tol 
    real(8), intent(out) :: x, err, iter 
    real(8) :: x_1
    
    !armo el primer x_n 
    x_1=x_0-f(x_0)/df(x_0) 
    iter=0
    !armo la iteración 
     
    do 
        if(abs(x_1-x_0)<tol .and. abs(f(x_1))<tol)then 
            x=x_1 
            err=abs(x_1-x_0)
            exit 
        else 
            x_0=x_1
            
        endif
        x_1=x_0-f(x_0)/df(x_0)
        iter=iter+1.0
    enddo 
end subroutine 

