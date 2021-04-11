program p3e5
    use constantese5
    implicit none
    real(8) :: x_0, x, df, f
    !x_0 es la semilla, x es el resultado o sea el tiempo al alcanzar el extremo de la rampa 
    x_i=0.0 
    v_i=0.0
    t_0=0.0
    tol=0.000001 
    n_step=100
    x_0=0.1 !semilla
    call newton(x_0, x)
    write(*,*)"El tiempo que tarda en llegar al extremo es" 
    write(*,*)x ,"s" 
!una vez obtenido el tiempo calculo la velocidad en ese tiempo 
    call funciones(x, f, df)
    write(*,*)"La velocidad en el extremo es"
    write(*,*)df, "m/s"


end program 

!-----------------------------------------------------

!-------------------------------------------------------
subroutine newton(x_0, x)
    !en esta subrutina busco la raiz de la ecuacuion x(t)-1.3 
    use constantese5
    implicit none 
    !real(8), intent(in) ::  tol 
    real(8), intent(out) :: x
    real(8) :: x_1, f , df, x_0
    call funciones(x_0, f, df) !para calcular la primer semilla
    !armo el primer x_n 
    x_1=x_0-f/df 
    
    !armo la iteración 
     
    do 
        !write(*,*)x_0, x_1, f 
        call funciones(x_1, f, df)
        if(abs(x_1-x_0)<tol .and. abs(f)<tol)then 
            x=x_1 
            
            exit 
        else 
            x_0=x_1
            
        endif
        !armo la proxima iteracion
        call funciones(x_0, f, df)
        x_1=x_0-f/df
       
    enddo 
end subroutine 

!------------------------------------------
subroutine funciones(t, f, df)
    !f corresponde a la posicion y df a la velocidad
    !en esta subruitina armo las funciones que se usaran en el metodo de newton, las funciones provienen de la ecuacion diferencial
    !calculadas con rk4, le das un tiempo y te calcula la velocidad y posicion para ese t
    use constantese5
    real(8) :: h, t, t_int, f, df
    real(8), dimension(2) :: y 
    integer :: i
    !condiciones iniciales
    h=t/n_step 
    t_int=t_0 
    y(1)=x_i
    y(2)=v_i
    !write(*,*)h, y
    do i=1, n_step 
        call rk4(t_int, y, h)
        !write(*,*)t_int, y
    enddo 
    f=y(1)-1.3 !posicion
    df=y(2) !velocidad
end subroutine 


    