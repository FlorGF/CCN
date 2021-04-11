subroutine histograma(r, s, x, c)
    use def_N 
    implicit none
    real(8), intent(in), dimension(N) :: r 
    integer, intent(in) :: s !recordar q s es la cantidad de barras del histograma 
    real(8), intent(out), dimension(s) :: x
    integer,intent(out),dimension(s) :: c
    real(8) :: min, max, h, aux2
    integer :: i, j, aux
    ! primero armo el arreglo x 
    !busco el valor mínimo 
    min=minval(r)
    !busco el valor máximo 
    max=maxval(r)
    !calculo el paso
    h=(max-min)/s
    !defino los valores del arreglo extremos
    x(1)=min+h/2 
    x(s)=max-h/2 
    do i=2, s-1
        x(i)=x(i-1)+h 
    enddo 
    
    !seteo c para que arranque en cero 
    do i=1, s 
        c(i)=0
    enddo 
    aux=0
    !armo el arreglo c
    do i=1, s !con este do voy marcando los intervalos 
        do j=1, N !recorro todos los valores de la lista de datos
            aux2=r(j)
            if(aux2>=x(i)-h/2 .and. aux2<=x(i)+h/2)then !me fijo si esta en el intervalo
               aux=aux+1 !si esta en el intervalo le sumo uno, sino salgo y pruebo con el siguiente intervalo
        
            endif 
        enddo 
        c(i)=aux 
        aux=0 !seteo en cero el contador 
    enddo 
   
    
end subroutine 