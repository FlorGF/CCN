module fitting 
    implicit none 
   contains 
   subroutine calculo(x_dat, y_dat,n_dat, n_param, a, z)
    integer, intent(in):: n_param, n_dat 
    real(8), intent(in) :: x_dat(n_dat), y_dat(n_dat)
    
    integer ::  i
    real(8), intent(out) :: a(n_param)
    real(8), intent(out) :: z(n_dat,n_param)
    real(8) :: ztz(n_param,n_param), ztz_inv(n_param,n_param), zt(n_param,n_dat)
    real(8) :: zty(n_param), aux(n_dat)

    
    
        do i=1, n_dat
            z(i,:)=fun(x_dat(i), n_param) !construyo la matriz z
        enddo 
        !write(*,*)z, "matriz z"
        zt=transpose(z) !estos calculos son para encontrar los parametros
        ztz=matmul(zt,z)
        !write(*,*)"ztz", ztz 
        write(*,*)"señal 3"
        call inverse(ztz,ztz_inv,n_param)
        write(*,*)"señal 4"
        zty=matmul(zt, y_dat)
        a=matmul(ztz_inv, zty) !arreglo de parametros 
        !write(*,*)"ztz_inv y zty"
        !write(*,*)ztz_inv
        !write(*,*)"----------------------"
        !write(*,*)zty 
        write(*,*)size(a), "tamaño de a", n_param, "n_param"
        write(*,*)"matriz a"
        write(*,*)a
        
        aux=matmul(z,a)



        write(*,*)"hola"
    end subroutine


!----------------------------------------------------------
   function fun(x, n_param)
    real(8), intent(in) :: x 
    integer :: n_param 
    real(8) :: fun(n_param)
    integer :: i 
    real(8), parameter :: pi=4.0*atan(2.0)
    do i=1, n_param 
        fun(i)=sin((2.0*i-1)*pi*x)
    enddo 
end function 

end module 