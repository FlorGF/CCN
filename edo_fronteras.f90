module edo_fronteras 
    implicit none 
contains 
!-------------------------------------------------
    subroutine disparo(x, y, yp, N, res)
        real(8), dimension(2) :: x, y, yp 
        integer(8) :: n !cantidad de pasos de integracion para cada disparo 
        real(8), dimension(n+1,3) :: res !aca vamos a guardar los resultados, primer columna x, segunda y, y tercera y' 
        !x tiene guardado los extremos, y tiene las condicioes de frontera e yp tiene las condiciones de la primer derivada en el 
        !extremo inferior (se eligen medio al azar)
        integer(8) :: i 
        real(8) :: h, t, a, b, y0 !a y b son los valores del extremo para las distintas derivadas primeras 
        real(8),dimension(2) ::  y_aux 
        h=(x(2)-x(1))/real(n) 
       

        !Primer disparo 
        !condiciones iniciales 
        t=x(1)
        y_aux(1)=y(1)
        y_aux(2)=yp(1)
        do i=1, N 
            call rk4(t, y_aux, h)
        enddo 
        a=y_aux(1) 
        write(*,*)a, "este es a" 


        !Segundo disparo 
        !condiciones iniciales 
        t=x(1)
        y_aux(1)=y(1)
        y_aux(2)=yp(2)
        do i=1, N 
            call rk4(t, y_aux, h)
        enddo 
        b=y_aux(1)
        write(*,*)b, "este es b" 

        !ahora calculo con regresión lineal el valor de la condición inicial que tendría que tener la derivada primera en el extremo
        !inferior para que me de la condicion en el extremo mayor 
        !y0=(yp(2)-yp(1))*(y(2)-yp(1))/(b-a)+a  (mio)
        y0=((Yp(2)-Yp(1))/(b-a))*(Y(2)-a)+Yp(1) !de ramiro
        

        !Tercer y último disparo 
        !condiciones iniciales 
        t=x(1)
        y_aux(1)=y(1)
        y_aux(2)=y0 

        !armo la primer fila de res 
        res(1,1)=t              !ext izq
        res(1,2)=y_aux(1)       !func ext izq
        res(1,3)=y_aux(2)       !deriva ext izq
        
        !calculo los valores que faltan 
    
        DO i=1, n
            CALL rk4(t, y_aux, h)
            res(i+1,1)=t
            res(i+1,2)=y_aux(1)
            res(i+1,3)=y_aux(2)
        enddo 
        
    end subroutine 
!-------------------------------------------------------------
    subroutine rk4(x, y, h)
        use deriv_p7
        implicit none 
        real(8), intent(inout) :: x
        real(8), intent(inout), dimension(2) :: y 
        real(8), intent(in) :: h !este es el paso 
        real(8), dimension(2) :: k1, k2, k3, k4 
    
        k1=deriv(x,y)
        k2=deriv(x+h/2.0,y+(h/2.0)*k1)
        k3=deriv(x+h/2.0,y+(h/2.0)*k2)
        k4=deriv(x+h,y+h*k3)
        y=y+(k1+2.0*(k2+k3)+k4)*h/6.0
        x=x+h
    end subroutine

!-----------------------------------------------------------------------
    subroutine dif_finitas_cte(x, y, n, res)
        integer :: n, i, j !n es el número de puntos interiores
        real(8), dimension(2) :: x, y !x e y igual que en el ejercicio anterior
        real(8), dimension(n+2,2) :: res !igual que en el anterior pero sin y' 
        real(8) :: Q, h 
        !Q es la constante de la ecuación diferencial
        !a partir de aca voy a inicializar los datos para armar la matriz A
        real(8), dimension(n,n) :: A, Ainv
        real(8), dimension(n) :: b, y_res 
        real(8) :: diag
        h=(x(2)-x(1))/real(n+1) 
        Q=0.15 !varia en todos los ejercicios (debería darlo por argumento)

        !armo la matriz a tridiagonal 
        diag=Q*h*h-2.0

        !seteo A en 0 (la mayoría de los valores de A son cero)
        do i=1, n 
            A(i,:)=0 
        enddo 
        !pongo los valores de la diagonal 
        do i=1, n 
            A(i,i)=diag 
        enddo 
        !armo las otras diagonales 
        do i=2, n-1 !varia las filas
            do j=2,n-1 !varia las columnas
                if(j/=i)cycle 
                A(i,j-1)=1.0 
                A(i,j+1)=1.0
            enddo 
        enddo 
        !pongo manualmente el primero y el último 
        A(1,2)=1
        A(n,n-1)=1 
        do i=1, n 
            write(*,*)A(i,:)
        enddo 

        !ahora armo la matriz b 
        !son todos ceros menos el primer y último valor 
        do i=1, n 
            b(i)=0.0 
        enddo 
        b(1)=-y(1)
        b(n)=-y(2)
        write(*,*)"b:", b 
        
        !calculo la inversa de A 
        call inversa(A, Ainv)
        write(*,*)"aca va Ainv" 
        do i=1, n 
            write(*,*)Ainv(i,:)
        enddo 
        !calculo los resultados 
        
        y_res=matmul(Ainv,b)
        write(*,*)"y_res:", y_res


        !armo la matriz resultados 
        res(1,1)=x(1)
        res(1,2)=y(1)
     
        
       
        do i=1, n 
            res(i+1,1)=x(1)+i*h
            res(i+1,2)=y_res(i)
        enddo 


    end subroutine 
    !--------------------------------------------------------------------------------
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





end module 