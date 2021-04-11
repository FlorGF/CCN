program p6e8 
    !use metodo_integrales 
    implicit none 
    real(8) :: a, b, masa, h1, i_int
    integer :: i
    real(8), dimension(3):: x
    
    a=-25.0
    b=25.0 
    
    !n1=3 !Cantidad de integrales 
    h1=(b-a)/2
    x(1)=a
    x(2)=a+h1
    x(3)=b
    !write(*,*)"el vector x", x 

    call integ_interna(x, i_int)
    masa=(h1*i_int*180)/3.0!180 es el valor de la densidad 

    write(*,*)"La masa es", masa 







end program

!------------------------------------------------------
subroutine integ_interna(x, i_int)
    use my_funcs_integ
    use metodo_integrales 
    implicit none 
    real(8), dimension(3):: x
    real(8), dimension(3) :: integ 
    real(8), dimension(3,2) :: ext 
    real(8) :: i_int 
    integer :: i, N 
    N=500
    do i=1, 3 
        ext(i,:)=g(x(i)) !defino los extremos de la integral, la función g es la función que esta en los extremos 
        write(*,*)ext(i,:), "extremo"
    enddo 
    
    do i=1, 3
        integ(i)=0.0
    enddo 
    do i=1, 3 
        call trapecio(ext(i,1), ext(i,2), integ(i), N)
    enddo 
   write(*,*)"integrales intternas", integ 
    i_int=integ(1)+4.0*integ(2)+integ(3) !hace el método de simpson simplificado (una sola iteración)
end subroutine 
        
