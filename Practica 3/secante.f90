subroutine secante(x_0, x_1, tol, x, err, iter)
    use funciones 
    implicit none 
    real(8), intent(inout) :: x_0, x_1, tol 
    real(8), intent(out) :: x, err, iter
    real(8) :: x_2 

    !defino el primero
     
    x_2=x_1-f(x_1)*(x_1-x_0)/(f(x_1)-f(x_0)) 
    iter=0

    do 
        x_2=x_1-f(x_1)*(x_1-x_0)/(f(x_1)-f(x_0)) 
        
        if(abs(x_2-x_1)<tol .and. abs(f(x_2))<tol)then 
            x=x_2 
            err=abs(x_2-x_1)
            
            exit 
        else 
            x_0=x_1 
            x_1=x_2 
        
            
        endif 
       ! x_2=x_1-f(x_1)*(x_1-x_0)/(f(x_1)-f(x_2)) 
        iter=iter+1.0
    enddo 

end subroutine 