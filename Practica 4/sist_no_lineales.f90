module sist_no_lineales
    implicit none 

contains 
subroutine puntofijo(r, x_sem, tol)
    use my_funcs_sistemas
    implicit none
    real(8), intent(out) :: r(order) !vetor solución 
    real(8) :: x_sem(order) !vector semilla 
    integer :: i 
    real(8), intent(in) :: tol
!seteo el primer valor
    r=f(x_sem) 
    do 
        
        if(norm2(r-x_sem)<tol)then 
            exit 
        else 
            x_sem=r
        endif 
        r=f(x_sem)
    enddo 

end subroutine 
subroutine gauss_seidel(r, x_sem, tol)
    use my_funcs_sistemas
    implicit none 
!la diferencia con este y el anterior esta en como calculas los nuevos x, en el anterior usas una semilla y calculas 
!todo el vector y en este calculas una componente y usas esta componente para calcular el que sigue es decir
!si tengo dos ecuaciones de x e y, la primer f me calcula una x y en la segunda f uso esa x para calcular y 
!asi que la diferencia esta en la funcion generatriz 
    real(8), intent(out) :: r(order) !vetor solución 
    real(8) :: x_sem(order) !vector semilla 
    integer :: i 
    real(8), intent(in) :: tol
!seteo el primer valor
    r=f(x_sem) 
    do 
        write(*,*)"olu"
        
        if(norm2(r-x_sem)<tol)then 
            exit 
        else 
            x_sem=r
        endif 
        r=f(x_sem)
    enddo 

end subroutine 

subroutine newton_raphson(r, x_sem, tol)
    use sistemas_de_ecuaciones
    use my_funcs_sistemas
    real(8), intent(out) :: r(order)
    real(8),dimension(order) :: x_sem, x_aux, FS
    real(8), dimension(order,order) :: Jinv, J
    real(8) :: tol 
    integer :: i 
    J=jacobiano(x_sem)
    !write(*,*)J , "jacobiano en la semilla"
    call inversa(J, Jinv)
    !write(*,*)Jinv, "inversa del jacobiano"
    FS=fun(x_sem)
    x_aux=x_sem-matmul(Jinv,FS) !sucesion
    do 
        write(*,*)"holi"
        if(norm2(x_aux-x_sem)<tol)then 
            r=x_aux 
            exit 
        else 
            x_sem=x_aux
        endif
        J=jacobiano(x_sem)
        call inversa(J, Jinv)
        FS=fun(x_sem)
        x_aux=x_sem-matmul(Jinv,FS)
    enddo 


    


end subroutine 
end module 