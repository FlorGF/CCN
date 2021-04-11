module sistemas_de_ecuaciones
    implicit none 
    integer(8), parameter :: n=3!la dimension de la matriz
    

contains 

!subrutina de gauss con pivoteo total 
subroutine gauus_total(A, B, X)
integer :: i,j
real(8), dimension (n,n) :: A
real(8), dimension (n) :: B
real(8), dimension(n,n+1) :: amp
real(8), dimension(n)::  X 
real(8) :: c, sum
!A matriz de coef, B matriz de ti, X matriz solucion 



!armo la matriz ampliada 
do i=1, n 
    amp(:,i)=A(:,i)
enddo 
amp(:,n+1)=B(:)

!armo la matriz de coeficientes
do i=1, n-1 
    if(amp(i,i)==0)then 
        write(*,*)"Error:cabecera nula" 
        stop 
    endif 
    do j=i+1, n 
        if(j==i)cycle 
        c=amp(j,i)/amp(i,i) 
        amp(j,:)=amp(j,:)-c*amp(i,:)
    enddo 
enddo 

!sustitucion hacia atras 
x(n)=amp(n,n+1)/amp(n,n) !la n esima solucion
do i=n-1, 1, -1 
    sum=0.0 !hay que setearlo todas las veces porque cambia de fila en fila
    do j=i+1, n
        sum=sum+amp(i,j)*x(j) !esto es la mult de la solucion anterior con el coef de la fila 
    enddo 
    x(i)=(amp(i,n+1)-sum)/amp(i,i) !la solucion i esima 
enddo  

end subroutine 

subroutine gauss_parcial(A, B, X) 
!resolucion por gauss con pivoteo parcial (lo unico que cambia es que elige de cabecera al elemento mas grande de la columna)
        integer :: i,j, cont
        real(8), dimension (n,n) :: A
        real(8), dimension (n) :: B
        real(8), dimension(n,n+1) :: amp
        real(8), dimension(n)::  X 
        real(8) :: c, sum, maxc
        real(8), dimension(n+1) :: aux
        !A matriz de coef, B matriz de ti, X matriz solucion 
        
        
        
        !armo la matriz ampliada 
        do i=1, n 
            amp(:,i)=A(:,i)
        enddo 
        amp(:,n+1)=B(:)  !armo la matriz de coeficientes


        do i=1, n-1  !armo los pivotes parciales
            maxc=abs(amp(i,i))
            cont=i
            do j=i+1, n 
            if(abs(amp(j,i))>maxc)then !me fijo si el pivote que esta asignado original es el mayor
                maxc=amp(j,i) !si no lo es guardo el que si es en max
                cont=j !cont indica en que fila esta el mayor
            endif
        enddo 
        aux=amp(i,:) !intercambia las filas
        amp(i,:)=amp(cont,:)
        amp(cont,:)=aux
            if(amp(i,i)==0)then 
                write(*,*)"Error:cabecera nula" 
                stop 
            endif 
            do j=i+1, n 
                if(j==i)cycle
                c=amp(j,i)/amp(i,i) 
                amp(j,:)=amp(j,:)-c*amp(i,:)
            enddo 
        enddo 
        
        !sustitucion hacia atras 
        x(n)=amp(n,n+1)/amp(n,n) !la n esima solucion
        do i=n-1, 1, -1 
            sum=0.0 !hay que setearlo todas las veces porque cambia de fila en fila
            do j=i+1, n
                sum=sum+amp(i,j)*x(j) !esto es la mult de la solucion anterior con el coef de la fila 
            enddo 
            x(i)=(amp(i,n+1)-sum)/amp(i,i) !la solucion i esima 
        enddo  


end subroutine 

subroutine gauss_jordan_total(A, B, X)
    integer :: i,j
    real(8), dimension (n,n) :: A
    real(8), dimension (n) :: B
    real(8), dimension(n,n+1) :: amp
    real(8), dimension(n)::  X 
    real(8) :: c, sum

    do i=1, n 
        amp(:,i)=A(:,i)
    enddo 
    amp(:,n+1)=B(:) 
    do i=1, n 
        if(amp(i,i)==0)then 
            write(*,*)"Error:cabecera nula" 
            stop 
        endif 
        do j=1, n 
            if(j==i)cycle
            c=amp(j,i)/amp(i,i) 
            amp(j,:)=amp(j,:)-c*amp(i,:)
        enddo 
        amp(i,:)=amp(i,:)/amp(i,i)
    enddo 
    x=amp(:,n+1) 
    
end subroutine 

subroutine jacobi(A,B,x_sem,x, tol)
    real(8), dimension(n,n) :: A 
    real(8), dimension(n) :: B, X, x_sem, x_old
    real(8) :: tol, err 
    integer :: i, j, k, iter_max
    !x_sem es el vector inicial solución 
    
    call dominante(A,B) !la matriz que se le aplique esto debe ser dominante 
    !write(*,*)A
    x_old=x_sem 
    k=0.0
    iter_max=1000
do
    !write(*,*)"hola"
    do i=1, n !de aca en adelante construyo el nuevo x, sabiendo que x1=(b-a12x2-a12x3)/a11 
        x(i)=b(i) 
        do j=1, n 
            if(j==i)cycle 
            x(i)=x(i)-a(i,j)*x_old(j)
        enddo 
        x(i)=x(i)/a(i,i) 
    enddo 
    if(norm2(x-x_old)<tol .or. k>iter_max)then !se fija si el x que calculo se acerca mas al resultado q el anterior 
        exit
    endif 
    x_old=x
    k=k+1
enddo 

end subroutine 
subroutine gauss_sidel(a, b, x_sem, x, tol)
    !no esta terminada, hay que corregir 
    real(8), dimension(n,n) :: A 
    real(8), dimension(n) :: B, X, x_sem, x_new
    real(8) :: tol, err 
    integer :: i, j, k, iter_max
    !x_sem es el vector inicial solución 
    call dominante(A,B) !la matriz que se le aplique esto debe ser dominante 
    x=x_sem 
    k=0.0
    iter_max=1000
    
do
    write(*,*)"hola"
    do i=1, n !de aca en adelante construyo el nuevo x, sabiendo que x1=(b-a12x2-a12x3)/a11 
        x_new(i)=b(i) 
        do j=1, n 
            if(j==i)cycle 
            x_new(i)=x(i)-a(i,j)*x(j)
        enddo 
        x_new(i)=x(i)/a(i,i) 
    enddo 
    if(norm2(x_new-x)<tol .or. k>iter_max)then !se fija si el x que calculo se acerca mas al resultado q el anterior 
        exit
    endif 
    x=x_new
    k=k+1
enddo 

end subroutine 

subroutine inversa(A, Ainv)
!obs:para q este programa compile hay q comentar el call dominante de las sub de arriba 
real(8), intent(in) :: A(:,:) !estos son arreglos automaticos--> toma la dimension como viene 
real(8), intent(out) :: Ainv(size(a,1),size(a,2))
real(8) :: Aamp(size(a,1),2*size(a,2))
integer :: i, j, n 
real(8) :: c
n=size(a,1) 
!genero la matriz super ampliada, que es la matriz A seguida de la identidad [A|I]
Aamp(1:n,1:n)=A !esto significa que las columnas de 1 a n y las filas de 1 a n de Aamp es igual a A 
!ahora hay que poner la parte de la identidad 
Aamp(:,n+1:2*n)=0.0 !seteo todos en cero 

!ahora ponemos los 1 en la diagonal 
do i=1, n
    Aamp(i,n+i)=1.0
enddo 

!y ahora hago gauss jordan aplicado a Aamp
do i=1, n 
    if(Aamp(i,i)==0)then 
        write(*,*)"Error:cabecera nula" 
        stop 
    endif 
    do j=1, n 
        if(j==i)cycle
        c=Aamp(j,i)/Aamp(i,i) 
        Aamp(j,:)=Aamp(j,:)-c*Aamp(i,:)
    enddo 
    Aamp(i,:)=Aamp(i,:)/Aamp(i,i)
enddo 
Ainv=Aamp(:,n+1:2*n)




end subroutine 
subroutine dominante(a,b)
    implicit none
    real(8), dimension(n,n) :: a
    real(8),dimension(n) :: aux, b
    integer :: i, k, j  
    real(8) :: sum , aux2 
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
                aux2=b(k)
                b(k)=b(i)
                b(i)=aux2
                exit
            elseif(i==n)then
                write(*,*)"La matriz no es diagonalmente dominante" 
                stop
            endif 
            enddo 
        enddo
    end subroutine 



end module 