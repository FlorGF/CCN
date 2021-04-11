subroutine dominante(a, n)
implicit none
real(8), dimension(n,n) :: a
real(8), dimension(n) :: aux
integer :: i, k, j, n
real(8) :: sum 
!de aca en adelante chequeamos si es dominante
    do k=1,n !elijo una fila
        do i=k, n !veo todas las filas menos la anterior para buscar con cual intercambio
            sum=0
            do j=1, n !recorro todos los números de la fila i
                if(k /= j)then
                sum=sum+abs(a(i,j))
                endif 
            enddo 
        if(abs(a(i,k)) > sum)then !chequeo si cumple
            aux(:)=a(k,:)            
            a(k,:)=a(i,:)   !aca intercambio
            a(i,:)=aux(:)
            exit
        elseif(i==n)then
            write(*,*)"La matriz no es diagonalmente dominante" 
            stop
        endif 
        enddo 
    enddo
end subroutine 