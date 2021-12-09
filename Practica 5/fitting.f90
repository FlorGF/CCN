module fitting
    use sistemas_de_ecuaciones
    use my_funcs_ajustes 
    implicit none 
    contains 

    subroutine ajuste_lienal(x, y, n_dat,n_param, a, err)
        integer :: i, n_dat, n_param 
        real(8), intent(in) :: x(n_dat), y(n_dat)
        real(8), intent(out) :: err 
        real(8), intent(out) :: a(n_param) 

        !este método consiste en encontrar los a's para eso hay que construir la matriz z, hacerle la transpuesta
        !y muchas cosas mas, la operacion que hay que hacer es (zt*z)^-1*zt*y
        !la matriz x son las funciones 
        real(8) :: z(n_dat,n_param), zt(n_param,n_dat), ztz(n_param,n_param)
        real(8) :: ztz_inv(n_param,n_param), zty(n_param) 

        !construyo la matriz z 
        do i=1, n_dat 
            z(i,:)=f(x(i),n_param)
        enddo 

        !calculo la transpuesta 
        zt=transpose(z)  
        ztz=matmul(zt,z) !la matriz de multiplicacion 

        !calculo la inversa de la matriz anterior 
        call inversa(ztz, ztz_inv)

        zty=matmul(zt, y)

        a=matmul(ztz_inv, zty)
        err=norm2(y-matmul(z,a))

        

    end subroutine 

    subroutine gauss_newton(x, y, n_dat, n_param, tol, a, a_sem)
    !este método sirve cuando no hay dependencia lineal entre las funciones
        integer, intent(in) :: n_dat, n_param
        real(8), intent(in) :: x(n_dat), y(n_dat), tol
        real(8) :: a_sem(n_param)
        real(8), intent(out) :: a(n_param)
        real(8) :: z(n_dat,n_param), zt(n_param,n_dat), ztz(n_param,n_param), deltaa(n_param)
        real(8) :: ztz_inv(n_param,n_param), zty(n_param), Fx(n_dat), D(n_dat)
        integer :: i, n_iter, cont
        !Fx es el vector de todas las funciones valuado en la semilla 

        !seteo los valores iniciales 
       ! D_old=y 
        n_iter=300
        do i=1, n_dat 
            z(i,:)=jacobiano(x(i),a_sem,n_param)
        enddo 
        !write(*,*)"z igual"
        !write(*,*)z 

        
        !stop 

        do i=1, n_dat 
            Fx(i)=f_n(x(i),a_sem, n_param)
        enddo 

        cont=0.0

do 
    
    D=y-Fx
    zt=transpose(z)  
    ztz=matmul(zt,z) 
    call inversa(ztz, ztz_inv)
    zty=matmul(zt, D)
    deltaa=matmul(ztz_inv, zty)
    write(*,*)"hola", deltaa 
    if(norm2(y-Fx)<tol .or. cont==n_iter)then 
        a=a_sem+deltaa
        exit 
    else 
       ! D_old=D_new 
        a_sem=a_sem+deltaa 
        cont=cont+1
    endif 
    do i=1, n_dat 
        z(i,:)=jacobiano(x(i),a_sem,n_param)
    enddo 
    do i=1, n_dat 
        Fx(i)=f_n(x(i),a_sem, n_param)
    enddo 

    

enddo 







    end subroutine 

end module